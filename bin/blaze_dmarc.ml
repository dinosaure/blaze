let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let stamp newline domain fpath value output =
  let oc, oc_close =
    match output with
    | None -> (stdout, ignore)
    | Some fpath ->
        let oc = open_out (Fpath.to_string fpath) in
        (oc, close_out) in
  let finally () = oc_close oc in
  Fun.protect ~finally @@ fun () ->
  let value =
    let new_line = match newline with `CRLF -> "\r\n" | `LF -> "\n" in
    Prettym.to_string ~new_line (Dmarc.Encoder.field ~receiver:domain) value
  in
  output_string oc (Dmarc.field_authentication_results :> string) ;
  output_string oc ": " ;
  output_string oc value ;
  let ic = open_in (Fpath.to_string fpath) in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let rec go buf =
    let len = input ic buf 0 (Bytes.length buf) in
    let str = Bytes.sub_string buf 0 len in
    output_string oc str ;
    if len > 0 then go buf in
  go (Bytes.create 0x7ff)

let verify _quiet newline domain dns fpath output =
  let ic = open_in (Fpath.to_string fpath) in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let buf = Bytes.create 0x7ff in
  let rec go decoder =
    match Dmarc.Verify.decode decoder with
    | #Dmarc.Verify.error as err -> error_msgf "%a" Dmarc.Verify.pp_error err
    | `Info value -> Ok value
    | `Query (decoder, dn, (Dns.Rr_map.K r as k)) ->
        Logs.debug (fun m -> m "ask %a:%a" Dns.Rr_map.ppk k Domain_name.pp dn) ;
        let resp = Dns_static.get_resource_record dns r dn in
        let decoder = Dmarc.Verify.response decoder r resp in
        go decoder
    | `Await decoder -> (
        let len = Stdlib.input ic buf 0 (Bytes.length buf) in
        match len with
        | 0 ->
            let decoder = Dmarc.Verify.src decoder String.empty 0 0 in
            go decoder
        | len when newline = `CRLF ->
            let str = Bytes.sub_string buf 0 len in
            let decoder = Dmarc.Verify.src decoder str 0 len in
            go decoder
        | len ->
            let str = Bytes.sub_string buf 0 len in
            let str = String.split_on_char '\n' str in
            let str = String.concat "\r\n" str in
            let len = String.length str in
            let decoder = Dmarc.Verify.src decoder str 0 len in
            go decoder) in
  let ( let* ) = Result.bind in
  let* info = go (Dmarc.Verify.decoder ()) in
  stamp newline domain fpath info output ;
  Ok ()

let verify quiet newline domain resolver fpath output =
  Miou_unix.run ~domains:0 @@ fun () ->
  let daemon, _he, dns = resolver () in
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () =
    Happy_eyeballs_miou_unix.kill daemon ;
    Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  match verify quiet newline domain dns fpath output with
  | Ok () -> `Ok ()
  | Error (`Msg msg) -> `Error (false, Fmt.str "%s." msg)

open Cmdliner
open Args

let input =
  let doc = "The email to check." in
  Arg.(required & pos 0 (some existing_file) None & info [] ~doc)

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

let output =
  let doc =
    "The path of the produced email with the new Authentication-Results field."
  in
  let new_file = Arg.conv (Fpath.of_string, Fpath.pp) in
  Arg.(value & opt (some new_file) None & info [ "o"; "output" ] ~doc)

let hostname =
  let parser =
    Angstrom.(parse_string ~consume:Consume.All) Emile.Parser.domain in
  let parser str =
    match parser str with
    | Ok _ as value -> value
    | Error _ -> error_msgf "Invalid domain: %S" str in
  let pp = Emile.pp_domain in
  Arg.conv (parser, pp)

let generate ~len =
  let res = Bytes.make len '\000' in
  for i = 0 to len - 1 do
    let chr =
      match Random.int (26 + 26 + 10) with
      | n when n < 26 -> Char.unsafe_chr (65 + n)
      | n when n < 26 + 26 -> Char.unsafe_chr (97 + n - 26)
      | n -> Char.unsafe_chr (48 + n - 26 - 26) in
    Bytes.set res i chr
  done ;
  Bytes.unsafe_to_string res

let default_hostname =
  let str = Unix.gethostname () in
  match (fst hostname) str with
  | `Ok domain -> domain
  | `Error _ ->
      let[@warning "-8"] (`Ok random_hostname : [ `Ok of _ | `Error of _ ]) =
        (fst hostname) (generate ~len:16) in
      Logs.warn (fun m ->
          m "Invalid default hostname: %S, use %a as the default hostname" str
            Emile.pp_domain random_hostname) ;
      random_hostname

let hostname =
  let doc = "Domain name of the machine." in
  let open Arg in
  value & opt hostname default_hostname & info [ "h"; "hostname" ] ~doc

let verify =
  let doc = "Verify DMARC informations & stamp an email with results." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) verifies DMARC informations & stamps the given email with \
         results.";
    ] in
  let open Term in
  let info = Cmd.info "verify" ~doc ~man in
  let term =
    const verify
    $ setup_logs
    $ newline
    $ hostname
    $ setup_resolver
    $ input
    $ output in
  Cmd.v info (ret term)

let default = Term.(ret (const (`Help (`Pager, None))))

let cmd =
  let doc = "A DMARC tool." in
  let man = [ `S Manpage.s_description ] in
  Cmd.group ~default (Cmd.info "dmarc" ~doc ~man) [ verify ]
