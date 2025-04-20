let ( % ) f g = fun x -> f (g x)

let request dns domain_name =
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

let verify dns newline stream =
  let decoder = Dmarc.Verify.decoder () in
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

let sign _quiet newline resolver seal msgsig keys receiver input =
  Miou_unix.run @@ fun () ->
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
  let prm0 = Miou.async @@ fun () -> verify dns newline stream0 in
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

let verify quiet newline resolver input =
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

let verify =
  let doc = "Verify ARC informations." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) verifies ARC informations & show results.";
    ] in
  let open Term in
  let info = Cmd.info "verify" ~doc ~man in
  let term = const verify $ setup_logs $ newline $ setup_resolver $ input in
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

let sign =
  let open Term in
  const sign
  $ setup_logs
  $ newline
  $ setup_resolver
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
