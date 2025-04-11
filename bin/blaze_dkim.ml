open Rresult

let ( % ) f g = fun x -> f (g x)
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let extra_servers = Hashtbl.create 0x100

let rem field_name lst =
  let fold (deleted, acc) x =
    if Mrmime.Field_name.equal field_name x && not deleted
    then (true, acc)
    else (deleted, x :: acc) in
  List.fold_left fold (false, []) lst |> fun (_, lst) -> List.rev lst

let show_fields verified =
  let rec merge acc l0 l1 =
    match (l0, l1) with
    | [], l1 -> List.rev_append acc l1
    | l0, [] -> List.rev_append acc l0
    | x :: r, l1 ->
        let l1 = rem x l1 in
        merge (x :: acc) r l1 in
  let fold acc (Dkim.Verify.Signature { dkim; _ }) =
    merge [] (Dkim.fields dkim) acc in
  let fields = List.fold_left fold [] verified in
  List.iter (Fmt.pr "%a\n%!" Mrmime.Field_name.pp) fields

let show_result verified expired errored =
  let show_verified (Dkim.Verify.Signature { dkim; _ }) =
    Fmt.pr "[%a]: %a\n%!"
      Fmt.(styled `Green string)
      "OK" Domain_name.pp (Dkim.domain dkim) in
  let show_expired dkim =
    Fmt.pr "[%a]: %a\n%!"
      Fmt.(styled `Yellow string)
      "EX" Domain_name.pp (Dkim.domain dkim) in
  let show_errored = function
    | `Invalid_domain_name dkim
    | `Invalid_domain_key dkim
    | `DNS_error dkim
    | `Invalid_DKIM_body_hash dkim ->
        Fmt.pr "[%a]: %a\n%!"
          Fmt.(styled `Red string)
          "ER" Domain_name.pp (Dkim.domain dkim) in
  List.iter show_verified verified ;
  List.iter show_expired expired ;
  List.iter show_errored errored

let response_of_dns_request errored ~dkim dns =
  match Dkim.Verify.domain_key dkim with
  | Error (`Msg msg) ->
      errored := `Invalid_domain_name dkim :: !errored ;
      Logs.err (fun m -> m "Invalid domain-name to retrive domain-key: %s" msg) ;
      `DNS_error (Fmt.str "Invalid domain-name for domain-key")
  | Ok domain_name -> begin
      Logs.debug (fun m -> m "DNS request to %a" Domain_name.pp domain_name) ;
      if Hashtbl.mem extra_servers domain_name
      then (
        let txts = Hashtbl.find extra_servers domain_name in
        match Dkim.domain_key_of_string txts with
        | Ok domain_key -> `Domain_key domain_key
        | Error (`Msg msg) ->
            Logs.err (fun m -> m "Invalid domain-key: %s" msg) ;
            errored := `Invalid_domain_key dkim :: !errored ;
            `DNS_error msg)
      else
        match Dns_static.getaddrinfo dns Dns.Rr_map.Txt domain_name with
        | Ok (_ttl, txts) ->
            let fn elt acc = elt :: acc in
            let txts = Dns.Rr_map.Txt_set.fold fn txts [] in
            let txts =
              List.map (String.concat "" % String.split_on_char ' ') txts in
            let txts = String.concat "" txts in
            begin
              match Dkim.domain_key_of_string txts with
              | Ok domain_key -> `Domain_key domain_key
              | Error (`Msg msg) ->
                  Logs.err (fun m -> m "Invalid domain-key: %s" msg) ;
                  errored := `Invalid_domain_key dkim :: !errored ;
                  `DNS_error msg
            end
        | Error (`Msg msg) ->
            Logs.err (fun m ->
                m "DNS error from %a: %s" Domain_name.pp domain_name msg) ;
            errored := `DNS_error dkim :: !errored ;
            `DNS_error msg
    end

let now () = Int64.of_float (Unix.gettimeofday ())

let expire dkim =
  match Dkim.expire dkim with None -> false | Some ts -> now () > ts

let verify quiet newline fields dns input =
  let ic, close =
    match input with
    | `File fpath -> (open_in (Fpath.to_string fpath), close_in)
    | `Stdin -> (stdin, ignore) in
  let finally () = close ic in
  Fun.protect ~finally @@ fun () ->
  let expired = ref [] in
  let errored = ref [] in
  let buf = Bytes.create 0x7ff in
  let rec go decoder =
    match Dkim.Verify.decode decoder with
    | `Malformed msg ->
        Logs.err (fun m -> m "Invalid email: %s" msg) ;
        error_msgf "Invalid email"
    | `Signatures sigs ->
        let fn (Dkim.Verify.Signature { dkim; fields; body = bh; _ } as s) =
          let _, Dkim.Hash_value (k, bh') = Dkim.signature_and_hash dkim in
          let bh' = Digestif.to_raw_string k bh' in
          if fields && Eqaf.equal bh bh'
          then Either.Left s
          else begin
            Logs.debug (fun m -> m "Invalid DKIM signature") ;
            Logs.debug (fun m -> m "Expected body hash: %s" (Base64.encode_exn bh')) ;
            Logs.debug (fun m -> m "Actual body hash:   %s" (Base64.encode_exn bh)) ;
            Logs.debug (fun m -> m "Signature of fields: %b" fields) ;
            Either.Right (`Invalid_DKIM_body_hash dkim)
          end in
        let sigs, errored' = List.partition_map fn sigs in
        errored := List.rev_append errored' !errored ;
        Ok sigs
    | `Query (decoder, dkim) when not (expire dkim) ->
        let response = response_of_dns_request errored ~dkim dns in
        let decoder = Dkim.Verify.response decoder ~dkim ~response in
        go decoder
    | `Query (decoder, dkim) ->
        let response = `Expired in
        expired := dkim :: !expired ;
        let decoder = Dkim.Verify.response decoder ~dkim ~response in
        go decoder
    | `Await decoder -> (
        let len = Stdlib.input ic buf 0 (Bytes.length buf) in
        match len with
        | 0 ->
            let decoder = Dkim.Verify.src decoder String.empty 0 0 in
            go decoder
        | len when newline = `CRLF ->
            let str = Bytes.sub_string buf 0 len in
            let decoder = Dkim.Verify.src decoder str 0 len in
            go decoder
        | len ->
            let str = Bytes.sub_string buf 0 len in
            let str = String.split_on_char '\n' str in
            let str = String.concat "\r\n" str in
            let decoder = Dkim.Verify.src decoder str 0 (String.length str) in
            go decoder) in
  let ( let* ) = Result.bind in
  let* verified = go (Dkim.Verify.decoder ()) in
  let expired = !expired in
  let errored = !errored in
  if (not quiet) && not fields
  then show_result verified expired errored
  else if (not quiet) && fields
  then show_fields verified ;
  match errored with [] -> Ok `Ok | _ -> Ok `Error

let extra_to_string pk =
  let pk = X509.Public_key.encode_der pk in
  Fmt.str "v=DKIM1; k=rsa; p=%s" (Base64.encode_string ~pad:true pk)

let verify quiet newline fields extra resolver input =
  Miou_unix.run ~domains:0 @@ fun () ->
  let daemon, _, dns = resolver () in
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () =
    Happy_eyeballs_miou_unix.kill daemon ;
    Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  let () =
    List.iter
      (fun (selector, v, extra) ->
        let domain_name =
          let open Domain_name in
          prepend_label v "_domainkey" >>= append selector in
        let domain_name = R.get_ok domain_name in
        Logs.debug (fun m ->
            m "add %a as a new DNS entry" Domain_name.pp domain_name) ;
        Hashtbl.add extra_servers domain_name (extra_to_string extra))
      extra in
  match verify quiet newline fields dns input with
  | Ok `Ok -> `Ok ()
  | Ok `Error -> `Error (false, "Invalid DKIM signature.")
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

let priv_of_seed seed =
  let g = Mirage_crypto_rng.(create ~seed (module Fortuna)) in
  Mirage_crypto_pk.Rsa.generate ~g ~bits:2048 ()

let pub_of_seed seed = Mirage_crypto_pk.Rsa.pub_of_priv (priv_of_seed seed)

let sign _verbose newline input output key selector fields hash canon
    domain_name =
  let ic, close_ic =
    match input with
    | `File fpath ->
        let ic = open_in (Fpath.to_string fpath) in
        (ic, close_in)
    | `Stdin -> (stdin, ignore) in
  let oc, close_oc =
    match output with
    | Some fpath -> (open_out (Fpath.to_string fpath), close_out)
    | None -> (stdout, ignore) in
  Fun.protect ~finally:(fun () -> close_ic ic) @@ fun () ->
  Fun.protect ~finally:(fun () -> close_oc oc) @@ fun () ->
  let dkim =
    Dkim.v ~selector ~fields ?hash ?canonicalization:canon domain_name in
  let buf = Bytes.create 0x7ff in
  let rec go signer =
    match Dkim.Sign.sign signer with
    | `Malformed msg ->
        Logs.err (fun m -> m "Invalid email: %s" msg) ;
        error_msgf "Invalid email"
    | `Signature dkim -> Ok dkim
    | `Await signer ->
        let len = Stdlib.input ic buf 0 (Bytes.length buf) in
        begin
          match len with
          | 0 ->
              let signer = Dkim.Sign.fill signer String.empty 0 0 in
              go signer
          | len when newline = `CRLF ->
              let str = Bytes.sub_string buf 0 len in
              let signer = Dkim.Sign.fill signer str 0 len in
              go signer
          | len ->
              let str = Bytes.sub_string buf 0 len in
              let str = String.split_on_char '\n' str in
              let str = String.concat "\r\n" str in
              let signer = Dkim.Sign.fill signer str 0 (String.length str) in
              go signer
        end in
  let ( let* ) = Result.bind in
  let* dkim = go (Dkim.Sign.signer ~key dkim) in
  let ppf = Format.formatter_of_out_channel oc in
  let dkim =
    let new_line = match newline with `CRLF -> "\r\n" | `LF -> "\n" in
    Prettym.to_string ~new_line Dkim.Encoder.as_field dkim in
  Fmt.pf ppf "%s%!" dkim ;
  seek_in ic 0 ;
  let rec go () =
    let len = Stdlib.input ic buf 0 (Bytes.length buf) in
    if len > 0
    then begin
      let str = Bytes.sub_string buf 0 len in
      output_string oc str ;
      go ()
    end in
  Ok (go ())

let sign _verbose newline input output private_key seed selector fields hash
    canon domain_name =
  Miou_unix.run ~domains:0 @@ fun () ->
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () = Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  match (seed, private_key) with
  | None, None -> `Error (true, "A private key or a seed is required.")
  | _, Some pk -> begin
      match
        sign _verbose newline input output pk selector fields hash canon
          domain_name
      with
      | Ok () -> `Ok ()
      | Error (`Msg msg) -> `Error (false, Fmt.str "%s." msg)
    end
  | Some (`Seed seed), None ->
      let pk = `Rsa (priv_of_seed seed) in
      begin
        match
          sign _verbose newline input output pk selector fields hash canon
            domain_name
        with
        | Ok () -> `Ok ()
        | Error (`Msg msg) -> `Error (false, Fmt.str "%s." msg)
      end

let gen seed output =
  Miou_unix.run ~domains:0 @@ fun () ->
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let oc, close =
    match output with
    | Some fpath -> (open_out (Fpath.to_string fpath), close_out)
    | None -> (stdout, ignore) in
  let finally () =
    Mirage_crypto_rng_miou_unix.kill rng ;
    close oc in
  Fun.protect ~finally @@ fun () ->
  let seed =
    match seed with
    | Some (`Seed seed) -> seed
    | None ->
        let str = Mirage_crypto_rng.generate 30 in
        Base64.encode_string ~pad:true str in
  let key = priv_of_seed seed in
  let pub = Mirage_crypto_pk.Rsa.pub_of_priv key in
  if oc == stdout
  then begin
    let pk =
      let cs = X509.Public_key.encode_der (`RSA pub) in
      Base64.encode_string ~pad:true cs in
    Fmt.pr "seed is %s\n%!" (Base64.encode_string ~pad:true seed) ;
    Fmt.pr "public key is %s\n%!" pk ;
    `Ok ()
  end
  else
    let pk = X509.Public_key.encode_pem (`RSA pub) in
    Fmt.pr "seed is %s\n%!" (Base64.encode_string ~pad:true seed) ;
    output_string oc pk ;
    `Ok ()

open Cmdliner
open Args

let parse_public_key str =
  match Fpath.of_string str with
  | Ok _ when Sys.file_exists str ->
      let ic = open_in str in
      let ln = in_channel_length ic in
      let rs = Bytes.create ln in
      really_input ic rs 0 ln ;
      close_in ic ;
      X509.Public_key.decode_pem (Bytes.unsafe_to_string rs)
  | _ -> Base64.decode str >>= X509.Public_key.decode_der

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
    let pk = Base64.encode_string ~pad:true (X509.Public_key.encode_der pk) in
    Fmt.pf ppf "%a:%a:%s" Domain_name.pp selector Domain_name.pp domain_name pk
  in
  Arg.conv (parser, pp)

let extra =
  let doc = "Extra entries of DKIM public keys." in
  Arg.(value & opt_all extra [] & info [ "e"; "extra" ] ~doc)

let fields =
  let doc = "Print which field are secured by the DKIM signatures." in
  Arg.(value & flag & info [ "fields" ] ~doc)

let input =
  let doc = "The email to verify." in
  Arg.(value & pos 0 existing_file_or_stdin `Stdin & info [] ~doc)

let setup_resolver happy_eyeballs_cfg nameservers local () =
  let happy_eyeballs =
    match happy_eyeballs_cfg with
    | None -> None
    | Some
        {
          aaaa_timeout;
          connect_delay;
          connect_timeout;
          resolve_timeout;
          resolve_retries;
        } ->
        Happy_eyeballs.create ?aaaa_timeout ?connect_delay ?connect_timeout
          ?resolve_timeout ?resolve_retries
          (Mtime_clock.elapsed_ns ())
        |> Option.some in
  let ( let* ) = Result.bind in
  let daemon, he = Happy_eyeballs_miou_unix.create ?happy_eyeballs () in
  let dns = Dns_static.create ~nameservers ~local he in
  let getaddrinfo dns record domain_name =
    match record with
    | `A ->
        let* ipaddr = Dns_static.gethostbyname dns domain_name in
        Ok Ipaddr.(Set.singleton (V4 ipaddr))
    | `AAAA ->
        let* ipaddr = Dns_static.gethostbyname6 dns domain_name in
        Ok Ipaddr.(Set.singleton (V6 ipaddr)) in
  Happy_eyeballs_miou_unix.inject he (getaddrinfo dns) ;
  (daemon, he, dns)

let setup_resolver =
  let open Term in
  const setup_resolver
  $ setup_happy_eyeballs
  $ setup_nameservers
  $ setup_dns_static

let verify =
  let doc = "Verify DKIM fields from the given email." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) verifies DKIM fiels from the given $(i,msgs).";
    ] in
  let open Term in
  let info = Cmd.info "verify" ~doc ~man in
  let term =
    const verify
    $ setup_logs
    $ newline
    $ fields
    $ extra
    $ setup_resolver
    $ input in
  Cmd.v info (ret term)

let input =
  let doc = "The email to sign." in
  Arg.(value & pos 0 existing_file_or_stdin `Stdin & info [] ~doc)

let new_file = Arg.conv (Fpath.of_string, Fpath.pp)

let output =
  let doc = "The path of the produced email with the new DKIM field." in
  Arg.(value & opt (some new_file) None & info [ "o"; "output" ] ~doc)

let private_key =
  let parser str =
    match Base64.decode ~pad:true str >>= X509.Private_key.decode_der with
    | Ok (`RSA key) -> Ok (`Rsa key)
    | Ok (`ED25519 key) -> Ok (`Ed25519 key)
    | Ok _ -> R.error_msgf "We handle only RSA key"
    | Error _ ->
    match Fpath.of_string str with
    | Error _ as err -> err
    | Ok _ when Sys.file_exists str -> (
        let ic = open_in str in
        let ln = in_channel_length ic in
        let rs = Bytes.create ln in
        really_input ic rs 0 ln ;
        let rs = Bytes.unsafe_to_string rs in
        match X509.Private_key.decode_pem rs with
        | Ok (`RSA key) -> Ok (`Rsa key)
        | Ok (`ED25519 key) -> Ok (`Ed25519 key)
        | Ok _ -> R.error_msgf "We handle only RSA key"
        | Error _ as err -> err)
    | Ok fpath -> R.error_msgf "%a does not exist" Fpath.pp fpath in
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
  let open Arg in
  value
  & opt_all field_name [ Mrmime.Field_name.from ]
  & info [ "f"; "field" ] ~doc

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
  let open Term in
  let info = Cmd.info "sign" ~doc ~man in
  let term =
    const sign
    $ setup_logs
    $ newline
    $ input
    $ output
    $ private_key
    $ seed
    $ selector
    $ fields
    $ hash
    $ canon
    $ hostname in
  Cmd.v info (ret term)

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
  Cmd.v (Cmd.info "gen" ~doc ~man) Term.(ret (const gen $ seed $ output))

let default = Term.(ret (const (`Help (`Pager, None))))

let cmd =
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
  Cmd.group ~default (Cmd.info "dkim" ~doc ~man) [ verify; sign; gen ]
