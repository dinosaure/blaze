let ( % ) f g = fun x -> f (g x)
let extra_servers = Hashtbl.create 0x100

let request dns domain_name =
  if Hashtbl.mem extra_servers domain_name
  then
    let txts = Hashtbl.find extra_servers domain_name in
    begin
      match Dkim.domain_key_of_string txts with
      | Ok domain_key -> (domain_name, `Domain_key domain_key)
      | Error (`Msg msg) -> (domain_name, `DNS_error msg)
    end
  else
    match Dns_static.getaddrinfo dns Dns.Rr_map.Txt domain_name with
    | Ok (_ttl, txts) ->
        let fn elt acc = elt :: acc in
        let txts = Dns.Rr_map.Txt_set.fold fn txts [] in
        let txts = List.map (String.concat "" % String.split_on_char ' ') txts in
        let txts = String.concat "" txts in
        begin
          match Dkim.domain_key_of_string txts with
          | Ok domain_key -> (domain_name, `Domain_key domain_key)
          | Error (`Msg msg) ->
              Logs.err (fun m -> m "Invalid domain-key: %s" msg) ;
              (domain_name, `DNS_error msg)
        end
    | Error (`Msg msg) ->
        Logs.err (fun m ->
            m "DNS error for %a: %s" Domain_name.pp domain_name msg) ;
        (domain_name, `DNS_error msg)

let verify dns newline ctx stream =
  let decoder = Dmarc.Verify.decoder ?ctx () in
  let rec go decoder =
    match Dmarc.Verify.decode decoder with
    | #Dmarc.Verify.error as err ->
        Logs.err (fun m -> m "Error from DMARC: %a" Dmarc.Verify.pp_error err) ;
        Error err
    | `Await decoder -> (
        match Bqueue.get stream with
        | None -> go (Dmarc.Verify.src decoder String.empty 0 0)
        | Some str when newline = `CRLF ->
            go (Dmarc.Verify.src decoder str 0 (String.length str))
        | Some str ->
            let str = String.split_on_char '\n' str in
            let str = String.concat "\r\n" str in
            go (Dmarc.Verify.src decoder str 0 (String.length str)))
    | `Info value -> Ok value
    | `Query (decoder, domain_name, Dns.Rr_map.K record) ->
        let response = Dns_static.get_resource_record dns record domain_name in
        let decoder = Dmarc.Verify.response decoder record response in
        go decoder in
  go decoder

let sign seal msgsig keys newline receiver queue results chain =
  let signer = Arc.Sign.signer ~seal ~msgsig ~receiver ~results keys chain in
  let msg = Queue.create () in
  let rec go t =
    match Arc.Sign.sign t with
    | `Await t -> (
        match Queue.pop queue with
        | str when newline = `CRLF ->
            Queue.add str msg ;
            go (Arc.Sign.fill t str 0 (String.length str))
        | str ->
            Queue.add str msg ;
            let str = String.split_on_char '\n' str in
            let str = String.concat "\r\n" str in
            go (Arc.Sign.fill t str 0 (String.length str))
        | exception Queue.Empty -> go (Arc.Sign.fill t String.empty 0 0))
    | `Malformed err -> Fmt.failwith "%s." err
    | `Set set ->
        let new_line = match newline with `CRLF -> "\r\n" | `LF -> "\n" in
        let str = Prettym.to_string ~new_line Arc.Encoder.stamp set in
        output_string stdout str ;
        Queue.iter (output_string stdout) msg in
  go signer

let chain dns newline stream =
  let ( let* ) = Result.bind in
  let rec go decoder =
    match Arc.Verify.decode decoder with
    | `Await decoder -> (
        match Bqueue.get stream with
        | None -> go (Arc.Verify.src decoder String.empty 0 0)
        | Some str when newline = `CRLF ->
            go (Arc.Verify.src decoder str 0 (String.length str))
        | Some str ->
            let str = String.split_on_char '\n' str in
            let str = String.concat "\r\n" str in
            go (Arc.Verify.src decoder str 0 (String.length str)))
    | `Queries (decoder, set) ->
        let* queries = Arc.Verify.queries set in
        let responses = List.map (request dns) queries in
        let* decoder = Arc.Verify.response decoder responses in
        go decoder
    | `Chain chain -> Ok chain
    | `Malformed err -> Error (`Msg err) in
  go (Arc.Verify.decoder ())

let sign _quiet newline resolver ctx seal msgsig keys receiver input =
  Miou_unix.run ~domains:0 @@ fun () ->
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let daemon, _, dns = resolver () in
  let finally () =
    Happy_eyeballs_miou_unix.kill daemon ;
    Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  let ic, ic_close =
    if input = "-" then (stdin, ignore) else (open_in input, close_in) in
  let finally () = ic_close ic in
  Fun.protect ~finally @@ fun () ->
  let stream0 = Bqueue.create 0x10 in
  let stream1 = Bqueue.create 0x10 in
  let msg = Queue.create () in
  let producer =
    Miou.async @@ fun () ->
    let buf = Bytes.create 0x7ff in
    let rec go () =
      let len = Stdlib.input ic buf 0 (Bytes.length buf) in
      if len > 0
      then (
        let str = Bytes.sub_string buf 0 len in
        Bqueue.put stream0 str ;
        Bqueue.put stream1 str ;
        Queue.push str msg ;
        go ())
      else (
        Bqueue.close stream0 ;
        Bqueue.close stream1) in
    go () in
  Miou.await_exn producer ;
  let prm0 = Miou.async @@ fun () -> verify dns newline ctx stream0 in
  let prm1 = Miou.async @@ fun () -> chain dns newline stream1 in
  let results = Miou.await_exn prm0 and chain = Miou.await_exn prm1 in
  match (results, chain) with
  | Ok results, Ok chain ->
      sign seal msgsig keys newline receiver msg results chain ;
      `Ok ()
  | Error err, _ -> `Error (false, Fmt.str "%a." Dmarc.Verify.pp_error err)
  | _, Error _ -> `Error (false, "Invalid email")

let pp_error ppf = function
  | `Invalid_email -> Fmt.string ppf "Invalid email"
  | `Msg msg -> Fmt.string ppf msg

let rec show_chain ppf = function
  | Arc.Verify.Nil from ->
      let from =
        {
          Emile.name = None;
          local = from.Emile.local;
          domain = from.Emile.domain;
        } in
      Emile.pp_mailbox Fmt.stdout from
  | Arc.Verify.Valid { next; set; _ } ->
      let domain_name = Arc.domain set in
      let uid = Arc.uid set in
      if Fmt.style_renderer ppf = `Ansi_tty
      then
        Fmt.pf ppf "%a@ %a %02d:%a" show_chain next
          Fmt.(styled (`Fg `Green) string)
          "->" uid
          Fmt.(styled `Bold Domain_name.pp)
          domain_name
      else if Fmt.utf_8 ppf
      then
        Fmt.pf ppf "%a@ -✓-> %02d:%a" show_chain next uid Domain_name.pp
          domain_name
      else
        Fmt.pf ppf "%a@ -[ok]-> %02d:%a" show_chain next uid Domain_name.pp
          domain_name
  | Arc.Verify.Broken (set, next) ->
      let domain_name = Arc.domain set in
      let uid = Arc.uid set in
      if Fmt.style_renderer ppf = `Ansi_tty
      then
        Fmt.pf ppf "%a@ %a %02d:%a" show_chain next
          Fmt.(styled (`Fg `Red) string)
          "->" uid
          Fmt.(styled `Bold Domain_name.pp)
          domain_name
      else if Fmt.utf_8 ppf
      then
        Fmt.pf ppf "%a@ -🞩-> %02d:%a" show_chain next uid Domain_name.pp
          domain_name
      else
        Fmt.pf ppf "%a@ -[err]-> %02d:%a" show_chain next uid Domain_name.pp
          domain_name

let verify quiet () newline resolver input =
  Miou_unix.run ~domains:0 @@ fun () ->
  let daemon, _he, dns = resolver () in
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () =
    Happy_eyeballs_miou_unix.kill daemon ;
    Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  let ic, close =
    match input with
    | "-" -> (stdin, ignore)
    | filename -> (open_in filename, close_in) in
  let finally () = close ic in
  Fun.protect ~finally @@ fun () ->
  let buf = Bytes.create 0x7ff in
  let ( let* ) = Result.bind in
  let rec go decoder =
    match Arc.Verify.decode decoder with
    | `Await decoder -> begin
        let len = Stdlib.input ic buf 0 (Bytes.length buf) in
        match len with
        | 0 -> go (Arc.Verify.src decoder String.empty 0 0)
        | len when newline = `CRLF ->
            let str = Bytes.sub_string buf 0 len in
            go (Arc.Verify.src decoder str 0 len)
        | len ->
            let str = Bytes.sub_string buf 0 len in
            let str = String.split_on_char '\n' str in
            let str = String.concat "\r\n" str in
            go (Arc.Verify.src decoder str 0 (String.length str))
      end
    | `Queries (decoder, set) ->
        let* queries = Arc.Verify.queries set in
        let responses = List.map (request dns) queries in
        let* decoder = Arc.Verify.response decoder responses in
        go decoder
    | `Chain chain -> Ok chain
    | `Malformed _ -> Error `Invalid_email in
  match go (Arc.Verify.decoder ()) with
  | Error err -> `Error (false, Fmt.str "%a." pp_error err)
  | Ok chain when not quiet ->
      Fmt.pr "@[<2>%a@]\n%!" show_chain chain ;
      `Ok ()
  | Ok chain ->
      let rec verify ok = function
        | Arc.Verify.Nil _ -> ok
        | Arc.Verify.Valid { next; _ } -> verify ok next
        | Arc.Verify.Broken (_, next) -> verify false next in
      if verify true chain then `Ok () else `Error (false, "Invalid ARC-chain.")

open Cmdliner
open Args

let input =
  let doc = "The email to verify. use $(b,-) for $(b,stdin)." in
  Arg.(value & pos 0 Args.file "-" & info [] ~doc)

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

let parse_public_key str =
  match Fpath.of_string str with
  | Ok _ when Sys.file_exists str ->
      let ic = open_in str in
      let ln = in_channel_length ic in
      let rs = Bytes.create ln in
      really_input ic rs 0 ln ;
      close_in ic ;
      X509.Public_key.decode_pem (Bytes.unsafe_to_string rs)
  | _ ->
      let ( let* ) = Result.bind in
      let* str = Base64.decode str in
      X509.Public_key.decode_der str

let extra_to_string pk =
  let pk = X509.Public_key.encode_der pk in
  Fmt.str "k=rsa; p=%s" (Base64.encode_string ~pad:true pk)
(* TODO(dinosaure): k=ed25519? *)

let extra =
  let parser str =
    match String.split_on_char ':' str with
    | [ selector; domain_name; pk ] -> (
        let selector = Domain_name.of_string selector in
        let domain_name =
          let ( let* ) = Result.bind in
          let* value = Domain_name.of_string domain_name in
          Domain_name.host value in
        let pk = parse_public_key pk in
        match (selector, domain_name, pk) with
        | Ok selector, Ok domain_name, Ok pk -> Ok (selector, domain_name, pk)
        | (Error _ as err), _, _
        | _, (Error _ as err), _
        | _, _, (Error _ as err) ->
            err)
    | _ -> error_msgf "Invalid format: %S" str in
  let pp ppf (selector, domain_name, pk) =
    let pk = Base64.encode_string ~pad:true (X509.Public_key.encode_der pk) in
    Fmt.pf ppf "%a:%a:%s" Domain_name.pp selector Domain_name.pp domain_name pk
  in
  Arg.conv (parser, pp)

let extra =
  let doc = "Extra entries of DKIM public keys." in
  Arg.(value & opt_all extra [] & info [ "e"; "extra" ] ~doc)

let setup_extra extra =
  List.iter
    (fun (selector, v, extra) ->
      let domain_name =
        let open Domain_name in
        let ( let* ) = Result.bind in
        let* v = prepend_label v "_domainkey" in
        append selector v in
      let domain_name = Result.get_ok domain_name in
      Logs.debug (fun m ->
          m "add %a as a new DNS entry" Domain_name.pp domain_name) ;
      Hashtbl.add extra_servers domain_name (extra_to_string extra))
    extra

let setup_extra = Term.(const setup_extra $ extra)

let verify =
  let doc = "Verify ARC informations." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) verifies ARC informations & show results.";
    ] in
  let open Term in
  let info = Cmd.info "verify" ~doc ~man in
  let term =
    const verify $ setup_logs $ setup_extra $ newline $ setup_resolver $ input
  in
  Cmd.v info (ret term)

let priv_of_seed ?(bits = 4096) (alg : Dkim.algorithm) seed : Dkim.key =
  match X509.Private_key.generate ~seed ~bits (alg :> X509.Key_type.t) with
  | #key as key -> key
  | _ -> assert false

let setup_keys bits seal_alg seed seal_key msgsig_key =
  match (seed, seal_key, msgsig_key) with
  | None, Some key, msgsig_key -> `Ok (key, msgsig_key)
  | Some seed, None, msgsig_key ->
      `Ok (priv_of_seed ?bits seal_alg seed, msgsig_key)
  | _, Some key, msgsig_key -> `Ok (key, msgsig_key)
  | None, None, _ ->
      `Error (true, "A private key or a seed is required to sign an email")

let bits =
  let doc = "Size of RSA key in bits." in
  Arg.(value & opt (some bits) None & info [ "b"; "bits" ] ~doc ~docv:"NUMBER")

let seed =
  let doc =
    "The seed (encoded in base64) used to generate an RSA key (with the \
     Fortuna random number generator)." in
  Arg.(value & opt (some base64) None & info [ "seed" ] ~doc ~docv:"SEED")

let seal_algorithm =
  let doc = "The algorithm use to encrypt/decrypt ARC-Set." in
  let open Arg in
  value & opt algorithm `RSA & info [ "a"; "algorithm" ] ~doc ~docv:"ALGORITHM"

let seal_key =
  let doc = "The key used to generate the $(i,Seal) signature." in
  let open Arg in
  value & opt (some private_key) None & info [ "seal" ] ~doc ~docv:"PRIVATE-KEY"

let msgsig_key =
  let doc = "The key used to generate the $(i,Message-Signature) signature." in
  let open Arg in
  value
  & opt (some private_key) None
  & info [ "signature" ] ~doc ~docv:"PRIVATE-KEY"

let setup_keys =
  let open Term in
  const setup_keys $ bits $ seal_algorithm $ seed $ seal_key $ msgsig_key |> ret

let setup_msgsig selector fields algorithm hash (default, key) canon domain_name
    =
  let key = Option.value ~default key in
  match (algorithm, key) with
  | `RSA, `RSA _ | `ED25519, `ED25519 _ ->
      let dkim =
        Dkim.v ~selector ~fields ~algorithm ?hash ?canonicalization:canon
          domain_name in
      `Ok dkim
  | _ ->
      let msg =
        "The algorithm used by the key is different from the one specified for \
         ARC-Message-Signature." in
      `Error (true, msg)

let default_hostname =
  let str = Unix.gethostname () in
  match Domain_name.of_string str with
  | Ok domain_name -> domain_name
  | Error (`Msg msg) -> Fmt.failwith "%s." msg

let hostname =
  let doc = "Domain name of the machine." in
  let open Arg in
  value
  & opt domain_name default_hostname
  & info [ "h"; "hostname" ] ~doc ~docv:"DOMAIN"

let msgsig_selector =
  let doc =
    "ARC-Message-Signature selector. A domain (see $(b,domain)) can store \
     several public-key. Each of them are identified by a $(i,selector) such \
     as the public-key is stored into $(i,selector)._domainkey.$(i,domain). It \
     can refer to a date, a location or an user. This selector is specific for \
     the ARC-Message-Signature field. The user must specify another selector \
     for the ARC-Seal field." in
  let open Arg in
  required & opt (some domain_name) None & info [ "signature-selector" ] ~doc

let field_name = Arg.conv (Mrmime.Field_name.of_string, Mrmime.Field_name.pp)

let fields =
  let doc = "Fields which will be used to generate the DKIM signature." in
  let open Arg in
  value
  & opt_all field_name [ Mrmime.Field_name.from ]
  & info [ "f"; "field" ] ~doc

let msgsig_hash =
  let doc =
    "Hash algorithm to digest header's fields and body. User can digest with \
     SHA1 or SHA256 algorithm." in
  Arg.(value & opt (some hash) None & info [ "signature-hash" ] ~doc)

let msgsig_algorithm =
  let doc = "The algorithm use to encrypt/decrypt fields." in
  let open Arg in
  value
  & opt algorithm `RSA
  & info [ "signature-algorithm" ] ~doc ~docv:"ALGORITHM"

let canon =
  let doc =
    "Canonicalization algorithm used to digest ARC-Set's fields and body. \
     Default value is $(i,relaxed/relaxed). A $(i,simple) canonicalization can \
     be used. The format of the argument is: $(i,CANON)/$(i,CANON) or \
     $(i,CANON) to use the same canonicalization for both header's fields and \
     body." in
  Arg.(value & opt (some canon) None & info [ "c" ] ~doc)

let setup_msgsig =
  let open Term in
  const setup_msgsig
  $ msgsig_selector
  $ fields
  $ msgsig_algorithm
  $ msgsig_hash
  $ setup_keys
  $ canon
  $ hostname
  |> ret

let setup_seal selector algorithm hash (key, _) domain_name =
  match (algorithm, key) with
  | `RSA, `RSA _ | `ED25519, `ED25519 _ ->
      let seal = Arc.Sign.seal ~algorithm ?hash ~selector domain_name in
      `Ok seal
  | _ ->
      let msg =
        "The algorithm used by the key is different from the one specified for \
         ARC-Message-Signature." in
      `Error (true, msg)

let seal_selector =
  let doc =
    "ARC-Seal selector. A domain (see $(b,domain)) can store several \
     public-key. Each of them are identified by a $(i,selector) such as the \
     public-key is stored into $(i,selector)._domainkey.$(i,domain). It can \
     refer to a date, a location or an user. This selector is specific for the \
     ARC-Seal field. The user must specify another selector for the \
     ARC-Message-Signature field." in
  let open Arg in
  required & opt (some domain_name) None & info [ "seal-selector" ] ~doc

let seal_hash =
  let doc =
    "Hash algorithm to digest ARC-Sets. User can digest with SHA1 or SHA256 \
     algorithm." in
  Arg.(value & opt (some hash) None & info [ "seal-hash" ] ~doc)

let setup_seal =
  let open Term in
  const setup_seal
  $ seal_selector
  $ seal_algorithm
  $ seal_hash
  $ setup_keys
  $ hostname
  |> ret

let to_domain domain_name =
  let segs = Domain_name.to_strings domain_name in
  `Domain segs

let input =
  let doc = "The email to sign. Use $(b,-) for $(b,stdin)." in
  let open Arg in
  value & pos 0 Args.file "-" & info [] ~doc ~docv:"FILE"

let setup_ctx sender helo ip =
  match (sender, helo, ip) with
  | None, None, None -> None
  | _ ->
      let none = Uspf.empty in
      let none =
        Option.fold ~none
          ~some:(fun helo -> Uspf.with_sender (`HELO helo) none)
          helo in
      let none =
        Option.fold ~none
          ~some:(fun sender -> Uspf.with_sender (`MAILFROM sender) none)
          sender in
      let none = Option.fold ~none ~some:(fun ip -> Uspf.with_ip ip none) ip in
      Some none

let sender =
  let parser str =
    let path =
      let ( let* ) = Result.bind in
      let* email = Emile.of_string str in
      Colombe_emile.to_path email in
    match path with
    | Ok v -> Ok v
    | Error _ -> error_msgf "Invalid sender: %S" str in
  let pp = Colombe.Path.pp in
  Arg.conv (parser, pp)

let sender =
  let doc = "The sender of the given email." in
  Arg.(value & opt (some sender) None & info [ "s"; "sender" ] ~doc)

let domain_name = Arg.conv (Domain_name.of_string, Domain_name.pp)

let helo =
  let doc = "HELO/EHLO name used by the SMTP client." in
  Arg.(value & opt (some domain_name) None & info [ "helo" ] ~doc)

let ip =
  let doc = "The IP address of the client." in
  let ipaddr = Arg.conv (Ipaddr.of_string, Ipaddr.pp) in
  Arg.(value & opt (some ipaddr) None & info [ "ip" ] ~doc)

let setup_ctx =
  let open Term in
  const setup_ctx $ sender $ helo $ ip

let sign =
  let open Term in
  const sign
  $ setup_logs
  $ newline
  $ setup_resolver
  $ setup_ctx
  $ setup_seal
  $ setup_msgsig
  $ setup_keys
  $ map to_domain hostname
  $ input
  |> ret

let sign =
  let doc = "Sign an email with a new ARC-Set." in
  let man =
    [
      `S Manpage.s_description; `P "$(tname) sign an email with a new ARC-Set.";
    ] in
  let info = Cmd.info "sign" ~doc ~man in
  Cmd.v info sign

let default = Term.(ret (const (`Help (`Pager, None))))

let cmd =
  let doc = "A ARC tool." in
  let man = [ `S Manpage.s_description ] in
  Cmd.group ~default (Cmd.info "arc" ~doc ~man) [ verify; sign ]
