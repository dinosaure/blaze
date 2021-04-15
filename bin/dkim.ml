open Rresult

module Caml_scheduler = Dkim.Sigs.Make (struct
  type +'a t = 'a
end)

module Caml_flow = struct
  type backend = Caml_scheduler.t

  type flow = in_channel

  let input flow buf off len =
    let res = input flow buf off len in
    Caml_scheduler.inj res
end

let extra_servers = Hashtbl.create 0x100

module Dns = struct
  include Dns_client_unix

  type backend = Caml_scheduler.t

  let getaddrinfo t `TXT domain_name =
    match Hashtbl.find extra_servers (Domain_name.to_string domain_name) with
    | str -> Caml_scheduler.inj (Ok [ str ])
    | exception Not_found ->
    match getaddrinfo t Dns.Rr_map.Txt domain_name with
    | Ok (_ttl, txtset) ->
        Caml_scheduler.inj (Ok (Dns.Rr_map.Txt_set.elements txtset))
    | Error _ as err -> Caml_scheduler.inj err
end

let caml =
  let open Caml_scheduler in
  { Dkim.Sigs.bind = (fun x f -> f (prj x)); return = inj }

let newline = Dkim.LF

let rem field_name lst =
  let fold (deleted, acc) x =
    if Mrmime.Field_name.equal field_name x && not deleted
    then (true, acc)
    else (deleted, x :: acc) in
  List.fold_left fold (false, []) lst |> fun (_, lst) -> List.rev lst

let show_fields valid =
  let rec merge acc l0 l1 =
    match (l0, l1) with
    | [], l1 -> List.rev_append acc l1
    | l0, [] -> List.rev_append acc l0
    | x :: r, l1 ->
        let l1 = rem x l1 in
        merge (x :: acc) r l1 in
  let fold acc dkim = merge [] (Dkim.fields dkim) acc in
  let fields = List.fold_left fold [] valid in
  List.iter (Fmt.pr "%a\n%!" Mrmime.Field_name.pp) fields

let show_result valid invalid =
  let show_valid dkim =
    Fmt.pr "[%a]: %a\n%!"
      Fmt.(styled `Green string)
      "OK" Domain_name.pp (Dkim.domain dkim) in
  let show_invalid dkim =
    Fmt.pr "[%a]: %a\n%!"
      Fmt.(styled `Red string)
      "ER" Domain_name.pp (Dkim.domain dkim) in
  List.iter show_valid valid ;
  List.iter show_invalid invalid

module Infix = struct
  let ( >>= ) = caml.bind

  let return = caml.return

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> return (Error err)
end

let verify quiet fields nameserver input =
  let dns = Dns_client_unix.create ?nameserver () in
  let ic, close =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let open Caml_scheduler in
  Dkim.extract_dkim ~newline ic caml (module Caml_flow) |> prj
  >>= fun ({ Dkim.prelude; dkim_fields; _ } as extracted) ->
  Dkim.extract_body ~newline ic caml (module Caml_flow) ~prelude |> prj |> R.ok
  >>= fun body ->
  let fold (valid, invalid) (dkim_field_name, dkim_field_value, m) =
    let fiber =
      let open Infix in
      Dkim.post_process_dkim m |> return >>? fun dkim ->
      Dkim.extract_server dns caml (module Dns) dkim >>? fun n ->
      Dkim.post_process_server n |> return >>? fun server ->
      return (Ok (dkim, server)) in
    match Caml_scheduler.prj fiber with
    | Error _ -> (valid, invalid)
    | Ok (dkim, server) ->
    match
      Dkim.verify extracted.Dkim.fields
        (dkim_field_name, dkim_field_value)
        dkim server body
    with
    | true -> (dkim :: valid, invalid)
    | false -> (valid, dkim :: invalid) in
  let valid, invalid = List.fold_left fold ([], []) dkim_fields in
  if (not quiet) && not fields
  then show_result valid invalid
  else if (not quiet) && fields
  then show_fields valid ;
  close ic ;
  match invalid with [] -> Ok 0 | _ -> Ok 1

let extra_to_string pk =
  let pk = X509.Public_key.encode_der pk in
  Fmt.str "v=DKIM1; k=rsa; p=%s"
    (Base64.encode_string ~pad:true (Cstruct.to_string pk))

let verify quiet fields nameserver extra input =
  let () =
    List.iter
      (fun (selector, v, extra) ->
        let domain_name =
          let open Domain_name in
          prepend_label v "_domainkey" >>= append selector in
        let domain_name = R.get_ok domain_name in
        let domain_name = Domain_name.to_string domain_name in
        Hashtbl.add extra_servers domain_name (extra_to_string extra))
      extra in
  match verify quiet fields nameserver input with
  | Ok n -> `Ok n
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

module Keep_flow = struct
  type backend = Caml_scheduler.t

  type flow = in_channel * Buffer.t

  let input (ic, bf) buf off len =
    let res = input ic buf off len in
    Buffer.add_subbytes bf buf off res ;
    Caml_scheduler.inj res
end

let priv_of_seed seed =
  let g =
    let seed = Cstruct.of_string seed in
    Mirage_crypto_rng.(create ~seed (module Fortuna)) in
  Mirage_crypto_pk.Rsa.generate ~g ~bits:2048 ()

let pub_of_seed seed = Mirage_crypto_pk.Rsa.pub_of_priv (priv_of_seed seed)

let sign _verbose input output key selector fields hash canon domain_name =
  let ic, length_ic, close_ic =
    match input with
    | Some fpath ->
        let ic = open_in (Fpath.to_string fpath) in
        (ic, in_channel_length ic, close_in)
    | None -> (stdin, 0x1000, ignore) in
  let oc, close_oc =
    match output with
    | Some fpath -> (open_out (Fpath.to_string fpath), close_out)
    | None -> (stdout, ignore) in
  let open Caml_scheduler in
  let buffer = Buffer.create length_ic in
  let dkim =
    Dkim.v ~selector ~fields ?hash ?canonicalization:canon domain_name in
  let dkim =
    Dkim.sign ~key ~newline (ic, buffer) caml (module Keep_flow) dkim |> prj
  in
  let ppf = Format.formatter_of_out_channel oc in
  let dkim = Prettym.to_string ~new_line:"\n" Dkim.Encoder.as_field dkim in
  Fmt.pf ppf "%s%!" dkim ;
  Fmt.pf ppf "%s%!" (Buffer.contents buffer) ;
  close_ic ic ;
  close_oc oc ;
  `Ok 0

let sign _verbose input output private_key seed selector fields hash canon
    domain_name =
  match (seed, private_key) with
  | None, None -> `Error (true, "A private key or a seed is required.")
  | _, Some (`RSA pk) ->
      sign _verbose input output pk selector fields hash canon domain_name
  | Some (`Seed seed), None ->
      let pk = priv_of_seed seed in
      sign _verbose input output pk selector fields hash canon domain_name

let gen seed output =
  let oc, close =
    match output with
    | Some fpath -> (open_out (Fpath.to_string fpath), close_out)
    | None -> (stdout, ignore) in
  let seed =
    match seed with
    | Some (`Seed seed) -> seed
    | None ->
        let () = Mirage_crypto_rng_unix.initialize () in
        let cs = Mirage_crypto_rng.generate 30 in
        Base64.encode_string ~pad:true (Cstruct.to_string cs) in
  let key = priv_of_seed seed in
  let pub = Mirage_crypto_pk.Rsa.pub_of_priv key in
  if oc == stdout
  then (
    let pk =
      let cs = X509.Public_key.encode_der (`RSA pub) in
      Base64.encode_string ~pad:true (Cstruct.to_string cs) in
    Fmt.pr "seed is %s\n%!" (Base64.encode_string ~pad:true seed) ;
    Fmt.pr "public key is %s\n%!" pk ;
    `Ok 0)
  else
    let pk = X509.Public_key.encode_pem (`RSA pub) in
    Fmt.pr "seed is %s\n%!" (Base64.encode_string ~pad:true seed) ;
    output_string oc (Cstruct.to_string pk) ;
    close oc ;
    `Ok 0

open Cmdliner

let inet_addr_of_string str =
  match Unix.inet_addr_of_string str with v -> Some v | exception _ -> None

let pp_nameserver ppf = function
  | `TCP, (inet_addr, 53) ->
      Fmt.pf ppf "tcp://%s/" (Unix.string_of_inet_addr inet_addr)
  | `UDP, (inet_addr, 53) ->
      Fmt.pf ppf "udp://%s/" (Unix.string_of_inet_addr inet_addr)
  | `TCP, (inet_addr, port) ->
      Fmt.pf ppf "tcp://%s:%d/" (Unix.string_of_inet_addr inet_addr) port
  | `UDP, (inet_addr, port) ->
      Fmt.pf ppf "udp://%s:%d/" (Unix.string_of_inet_addr inet_addr) port

let nameserver =
  let parser str =
    let uri = Uri.of_string str in
    let via =
      match Uri.scheme uri with
      | None | Some "udp" -> `UDP
      | Some "tcp" -> `TCP
      | Some scheme -> Fmt.invalid_arg "Invalid scheme: %S" scheme in
    match (Option.bind (Uri.host uri) inet_addr_of_string, Uri.port uri) with
    | None, None -> None
    | None, Some port -> Some (via, (Unix.inet_addr_loopback, port))
    | Some inet_addr, None -> Some (via, (inet_addr, 53))
    | Some inet_addr, Some port -> Some (via, (inet_addr, port)) in
  let parser str =
    match parser str with
    | Some v -> Ok v
    | None -> R.error_msgf "Invalid nameserver: %a" Uri.pp (Uri.of_string str)
    | exception _ -> R.error_msgf "Invalid nameserver: %S" str in
  Arg.conv (parser, pp_nameserver)

let parse_public_key str =
  match Fpath.of_string str with
  | Ok _ when Sys.file_exists str ->
      let ic = open_in str in
      let ln = in_channel_length ic in
      let rs = Bytes.create ln in
      really_input ic rs 0 ln ;
      close_in ic ;
      X509.Public_key.decode_pem (Cstruct.of_bytes rs)
  | _ -> Base64.decode str >>| Cstruct.of_string >>= X509.Public_key.decode_der

let extra =
  let parser str =
    match String.split_on_char ':' str with
    | [ selector; domain_name; pk ] -> (
        let selector = Domain_name.of_string selector in
        let domain_name =
          Domain_name.of_string domain_name >>= Domain_name.host in
        let pk = parse_public_key pk in
        match (selector, domain_name, pk) with
        | Ok selector, Ok domain_name, Ok pk -> Ok (selector, domain_name, pk)
        | (Error _ as err), _, _
        | _, (Error _ as err), _
        | _, _, (Error _ as err) ->
            err)
    | _ -> R.error_msgf "Invalid format: %S" str in
  let pp ppf (selector, domain_name, pk) =
    let pk =
      Base64.encode_string ~pad:true
        (Cstruct.to_string (X509.Public_key.encode_der pk)) in
    Fmt.pf ppf "%a:%a:%s" Domain_name.pp selector Domain_name.pp domain_name pk
  in
  Arg.conv (parser, pp)

let extra =
  let doc = "Extra entries of DKIM public keys." in
  Arg.(value & opt_all extra [] & info [ "e"; "extra" ] ~doc)

let nameserver =
  let doc = "DNS nameserver." in
  Arg.(value & opt (some nameserver) None & info [ "n"; "nameserver" ] ~doc)

let fields =
  let doc = "Print which field are secured by the DKIM signatures." in
  Arg.(value & flag & info [ "fields" ] ~doc)

let common_options = "COMMON OPTIONS"

let verbosity =
  let env = Arg.env_var "BLAZE_LOGS" in
  Logs_cli.level ~docs:common_options ~env ()

let renderer =
  let env = Arg.env_var "BLAZE_FMT" in
  Fmt_cli.style_renderer ~docs:common_options ~env ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr) ;
  Option.is_none level

let setup_logs = Term.(const setup_logs $ renderer $ verbosity)

let existing_file =
  let parser = function
    | "-" -> Ok None
    | str ->
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok (Some v)
    | Ok v -> Rresult.R.error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.option ~none:(Fmt.any "-") Fpath.pp)

let input =
  let doc = "The email to verify." in
  Arg.(value & pos 0 existing_file None & info [] ~doc)

let verify =
  let doc = "Verify DKIM fields from the given email." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) verifies DKIM fiels from the given $(i,msgs).";
    ] in
  ( Term.(ret (const verify $ setup_logs $ fields $ nameserver $ extra $ input)),
    Term.info "verify" ~doc ~man )

let input =
  let doc = "The email to sign." in
  Arg.(value & pos 0 existing_file None & info [] ~doc)

let new_file = Arg.conv (Fpath.of_string, Fpath.pp)

let output =
  let doc = "The path of the produced email with the new DKIM field." in
  Arg.(value & opt (some new_file) None & info [ "o"; "output" ] ~doc)

let private_key =
  let parser str =
    match
      Base64.decode ~pad:true str
      >>| Cstruct.of_string
      >>= X509.Private_key.decode_der
    with
    | Ok _ as v -> v
    | Error _ ->
    match Fpath.of_string str with
    | Ok _ when Sys.file_exists str ->
        let ic = open_in str in
        let ln = in_channel_length ic in
        let rs = Bytes.create ln in
        really_input ic rs 0 ln ;
        let rs = Bytes.unsafe_to_string rs in
        X509.Private_key.decode_pem (Cstruct.of_string rs)
    | Ok fpath -> R.error_msgf "%a does not exist" Fpath.pp fpath
    | Error _ as err -> err in
  let pp ppf _pk = Fmt.pf ppf "<private-key>" in
  Arg.conv (parser, pp)

let domain_name = Arg.conv (Domain_name.of_string, Domain_name.pp)

let hash =
  let parser str =
    match String.(trim (lowercase_ascii str)) with
    | "sha1" -> Ok `SHA1
    | "sha256" -> Ok `SHA256
    | _ -> R.error_msgf "Invalid hash: %S" str in
  let pp ppf = function
    | `SHA1 -> Fmt.string ppf "sha1"
    | `SHA256 -> Fmt.string ppf "sha256" in
  Arg.conv (parser, pp)

let canon =
  let parser str =
    let v = String.trim str in
    let v = String.lowercase_ascii v in
    match String.split_on_char '/' v with
    | [ "simple"; "simple" ] | [] | [ "simple" ] -> Ok (`Simple, `Simple)
    | [ "simple"; "relaxed" ] -> Ok (`Simple, `Relaxed)
    | [ "relaxed"; "simple" ] -> Ok (`Relaxed, `Simple)
    | [ "relaxed"; "relaxed" ] | [ "relaxed" ] -> Ok (`Relaxed, `Relaxed)
    | _ -> Rresult.R.error_msgf "Invalid canonicalization specification: %S" str
  in
  let pp ppf = function
    | `Simple, `Simple -> Fmt.string ppf "simple"
    | `Relaxed, `Relaxed -> Fmt.string ppf "relaxed"
    | `Simple, `Relaxed -> Fmt.string ppf "simple/relaxed"
    | `Relaxed, `Simple -> Fmt.string ppf "relaxed/simple" in
  Arg.conv (parser, pp)

let seed =
  let parser str =
    match Base64.decode ~pad:true str with
    | Ok v -> Ok (`Seed v)
    | Error _ as err -> err in
  let pp ppf (`Seed v) = Fmt.string ppf (Base64.encode_string ~pad:true v) in
  Arg.conv (parser, pp)

let field_name = Arg.conv (Mrmime.Field_name.of_string, Mrmime.Field_name.pp)

let private_key =
  let doc = "The X.509 PEM encoded private key used to sign the email." in
  Arg.(value & opt (some private_key) None & info [ "p" ] ~doc)

let seed =
  let doc =
    "Seed to generate a private key. Instead to pass a private-key, the user \
     can give a seed used then by a Fortuna random number generator to \
     generate a RSA private-key. From the seed, the user is able to reproduce \
     the same RSA private-key (and the public-key). " in
  Arg.(value & opt (some seed) None & info [ "seed" ] ~doc)

let fields =
  let doc = "Fields which will be used to generate the DKIM signature." in
  Arg.(
    value
    & opt_all field_name [ Mrmime.Field_name.from ]
    & info [ "f"; "field" ] ~doc)

let selector =
  let doc =
    "DKIM selector. A domain (see $(b,domain)) can store several public-key. \
     Each of them are identified by a $(i,selector) such as the public-key is \
     stored into $(i,selector)._domainkey.$(i,domain). It can refer to a date, \
     a location or an user." in
  Arg.(required & opt (some domain_name) None & info [ "s"; "selector" ] ~doc)

let hash =
  let doc =
    "Hash algorithm to digest header's fields and body. User can digest with \
     SHA1 or SHA256 algorithm." in
  Arg.(value & opt (some hash) None & info [ "hash" ] ~doc)

let canon =
  let doc =
    "Canonicalization algorithm used to digest header's fields and body. \
     Default value is $(i,relaxed/relaxed). A $(i,simple) canonicalization can \
     be used. The format of the argument is: $(i,canon)/$(i,canon) or \
     $(i,canon) to use the same canonicalization for both header's fields and \
     body." in
  Arg.(value & opt (some canon) None & info [ "c" ] ~doc)

let hostname =
  let doc =
    "The domain where the DNS TXT record is available (which contains the \
     public-key)." in
  Arg.(required & opt (some domain_name) None & info [ "h"; "hostname" ] ~doc)

let sign =
  let doc = "Sign the given email and put a new DKIM field." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) signs the given $(i,msgs) and put a new DKIM field.";
    ] in
  ( Term.(
      ret
        (const sign
        $ setup_logs
        $ input
        $ output
        $ private_key
        $ seed
        $ selector
        $ fields
        $ hash
        $ canon
        $ hostname)),
    Term.info "sign" ~doc ~man )

let output =
  let doc = "The path of the produced PEM-encoded public key." in
  Arg.(value & pos 0 (some new_file) None & info [] ~doc)

let seed =
  let parser str =
    match Base64.decode ~pad:true str with
    | Ok v -> Ok (`Seed v)
    | Error _ as err -> err in
  let pp ppf (`Seed v) = Fmt.string ppf (Base64.encode_string ~pad:true v) in
  Arg.conv (parser, pp)

let seed =
  let doc = "Seed for private key." in
  Arg.(value & opt (some seed) None & info [ "seed" ] ~doc)

let gen =
  let doc =
    "Generate a public RSA key and a seed to reproduce the private key." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) generates a new RSA key from a seed (optional).";
    ] in
  (Term.(ret (const gen $ seed $ output)), Term.info "gen" ~doc ~man)

let default =
  let doc = "A tool to manipulate DKIM fields." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Use $(tname) $(i,verify) to verify DKIM fields from the given \
         $(i,msgs).";
      `P
        "Use $(tname) $(i,sign) to sign the given $(i,msgs) with a new DKIM \
         field.";
    ] in
  (Term.(ret (const (`Help (`Pager, None)))), Term.info "dkim" ~doc ~man)

let () = Term.(exit_status @@ eval_choice default [ verify; sign; gen ])
