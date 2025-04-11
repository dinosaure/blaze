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
    | `File fpath -> (open_in (Fpath.to_string fpath), close_in)
    | `Stdin -> (stdin, ignore) in
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

let default = Term.(ret (const (`Help (`Pager, None))))

let cmd =
  let doc = "A ARC tool." in
  let man = [ `S Manpage.s_description ] in
  Cmd.group ~default (Cmd.info "arc" ~doc ~man) [ verify ]
