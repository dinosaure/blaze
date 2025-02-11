open Rresult

let ctx sender helo ip =
  Uspf.empty |> fun ctx ->
  Option.fold ~none:ctx
    ~some:(fun helo -> Uspf.with_sender (`HELO helo) ctx)
    helo
  |> fun ctx ->
  Option.fold ~none:ctx
    ~some:(fun sender -> Uspf.with_sender (`MAILFROM sender) ctx)
    sender
  |> fun ctx -> Option.fold ~none:ctx ~some:(fun ip -> Uspf.with_ip ip ctx) ip

let rec transmit ic oc =
  let tmp = Bytes.create 0x1000 in
  go tmp ic oc

and go tmp ic oc =
  let len = input ic tmp 0 (Bytes.length tmp) in
  if len > 0
  then (
    output oc tmp 0 len ;
    go tmp ic oc)

let unstrctrd_to_utf_8_string_with_lf l =
  let buf = Buffer.create 0x100 in
  let f = function
    | `CR -> Buffer.add_char buf '\r'
    | `FWS (wsp : Unstrctrd.wsp) ->
        Buffer.add_char buf '\n' ;
        Buffer.add_string buf (wsp :> string)
    | `LF | `Invalid_char _ -> ()
    | `OBS_NO_WS_CTL (chr : Unstrctrd.obs) -> Buffer.add_char buf (chr :> char)
    | `Uchar uchar -> Uutf.Buffer.add_utf_8 buf uchar
    | `WSP (wsp : Unstrctrd.wsp) -> Buffer.add_string buf (wsp :> string)
    | `d0 -> Buffer.add_char buf '\000' in
  Unstrctrd.iter ~f l ;
  Buffer.contents buf

let check dns ctx =
  let eval : type a. dns:Dns_static.t -> a Uspf.t -> Uspf.Result.t option =
   fun ~dns t ->
    let rec go : type a. a Uspf.t -> a = function
      | Request (domain_name, record, fn) ->
          let resp = Dns_static.get_resource_record dns record domain_name in
          go (fn resp)
      | Return v -> v
      | Map (x, fn) -> fn (go x)
      | Tries lst -> List.iter (fun fn -> go (fn ())) lst
      | Choose_on c ->
      try go (c.fn ())
      with Uspf.Result result ->
        let none _ = Uspf.terminate result in
        let some = Fun.id in
        let fn =
          match result with
          | `None -> Option.fold ~none ~some c.none
          | `Neutral -> Option.fold ~none ~some c.neutral
          | `Fail -> Option.fold ~none ~some c.fail
          | `Softfail -> Option.fold ~none ~some c.softfail
          | `Temperror -> Option.fold ~none ~some c.temperror
          | `Permerror -> Option.fold ~none ~some c.permerror
          | `Pass m -> begin
              fun () -> match c.pass with Some fn -> fn m | None -> none ()
            end in
        go (fn ()) in
    match go t with exception Uspf.Result result -> Some result | _ -> None
  in
  eval ~dns (Uspf.get_and_check ctx)

let extract_received_spf ?(newline = `LF) ic =
  let buf = Bytes.create 0x7ff in
  let rec go extract =
    match Uspf.Extract.extract extract with
    | `Fields fields -> Ok fields
    | `Malformed _ -> R.error_msgf "Invalid email"
    | `Await extract ->
    match input ic buf 0 (Bytes.length buf) with
    | 0 -> go (Uspf.Extract.src extract "" 0 0)
    | len when newline = `CRLF ->
        go (Uspf.Extract.src extract (Bytes.sub_string buf 0 len) 0 len)
    | len ->
        let str = Bytes.sub_string buf 0 len in
        let str = String.split_on_char '\n' str in
        let str = String.concat "\r\n" str in
        let len = String.length str in
        go (Uspf.Extract.src extract str 0 len) in
  go (Uspf.Extract.extractor ())

let impossible_to_stamp =
  `Error (false, "Impossible to stamp the incoming email with Received-SPF")

let stamp quiet hostname resolver sender helo ip input output =
  Miou_unix.run ~domains:0 @@ fun () ->
  let daemon, _he, dns = resolver () in
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () =
    Happy_eyeballs_miou_unix.kill daemon ;
    Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  let ic, close_ic =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let oc, close_oc =
    match output with
    | Some fpath -> (open_out (Fpath.to_string fpath), close_out)
    | None -> (stdout, ignore) in
  let ctx = ctx sender helo ip in
  match check dns ctx with
  | Some res when quiet -> begin
      match res with
      | `Pass _ | `None | `Neutral -> `Ok ()
      | `Fail | `Softfail | `Permerror | `Temperror -> impossible_to_stamp
    end
  | Some res ->
      let field_name, unstrctrd = Uspf.to_field ~ctx ~receiver:hostname res in
      Fmt.pr "%a: %s\n%!" Mrmime.Field_name.pp field_name
        (unstrctrd_to_utf_8_string_with_lf unstrctrd) ;
      transmit ic oc ;
      close_ic ic ;
      close_oc oc ;
      begin
        match res with
        | `Pass _ | `None | `Neutral -> `Ok ()
        | _ (* Fail | Softfail | Permerror | Temperror *) -> impossible_to_stamp
      end
  | None -> impossible_to_stamp

let to_exit_code results =
  let res = ref true in
  let f (_, _, a, b) =
    match (a, b) with
    | `Pass, `Pass _
    | `Fail, `Fail
    | `None, `None
    | `Permerror, `Permerror
    | `Temperror, `Temperror
    | `Neutral, `Neutral
    | `Softfail, `Softfail ->
        ()
    | _ -> res := false in
  List.iter f results ;
  if !res then `Ok () else `Error (false, "Invalid SPF source.")

let pp_expected ppf = function
  | `Pass -> Fmt.(styled `Green string) ppf "pass"
  | `Fail -> Fmt.(styled `Red string) ppf "fail"
  | `None -> Fmt.(styled `Faint string) ppf "none"
  | `Neutral -> Fmt.(styled `Blue string) ppf "neutral"
  | `Softfail -> Fmt.(styled `Yellow string) ppf "softfail"
  | `Permerror -> Fmt.(styled `Red string) ppf "permerror"
  | `Temperror -> Fmt.(styled `Red string) ppf "temperror"

let pp_result ppf = function
  | `Pass _ -> Fmt.(styled `Green string) ppf "pass"
  | `Fail -> Fmt.(styled `Red string) ppf "fail"
  | `None -> Fmt.(styled `Faint string) ppf "none"
  | `Neutral -> Fmt.(styled `Blue string) ppf "neutral"
  | `Softfail -> Fmt.(styled `Yellow string) ppf "softfail"
  | `Permerror -> Fmt.(styled `Red string) ppf "permerror"
  | `Temperror -> Fmt.(styled `Red string) ppf "temperror"

let show_results results =
  let f = function
    | Some sender, Some ip, expected, result ->
        Fmt.pr "%a from %a: %a (expected: %a)\n%!" Emile.pp_mailbox sender
          Ipaddr.pp ip pp_result result pp_expected expected
    | Some sender, None, expected, result ->
        Fmt.pr "%a: %a (expected: %a)\n%!" Emile.pp_mailbox sender pp_result
          result pp_expected expected
    | None, Some ip, expected, result ->
        Fmt.pr "%a: %a (expected: %a)\n%!" Ipaddr.pp ip pp_result result
          pp_expected expected
    | None, None, expected, result ->
        Fmt.pr "unidentified sender: %a (expected %a)\n%!" pp_result result
          pp_expected expected in
  List.iter f results ;
  `Ok ()
(* XXX(dinosaure): [to_exit_codes results]? *)

let analyze quiet resolver input =
  Miou_unix.run ~domains:0 @@ fun () ->
  let daemon, _he, dns = resolver () in
  let ic, close_ic =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () =
    Happy_eyeballs_miou_unix.kill daemon ;
    Mirage_crypto_rng_miou_unix.kill rng ;
    close_ic ic in
  Fun.protect ~finally @@ fun () ->
  let res = extract_received_spf ~newline:`LF ic in
  match res with
  | Ok extracted ->
      let results =
        List.fold_left
          (fun acc { Uspf.Extract.result; ctx; sender; ip; _ } ->
            match check dns ctx with
            | Some v -> (sender, ip, result, v) :: acc
            | _ -> acc)
          [] extracted in
      if quiet then to_exit_code results else show_results results
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

open Cmdliner
open Args

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

let existing_file =
  let parser = function
    | "-" -> Ok None
    | str ->
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok (Some v)
    | Ok v -> R.error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.option ~none:(Fmt.any "-") Fpath.pp)

let input =
  let doc = "The email to check." in
  Arg.(value & pos 0 existing_file None & info [] ~doc)

let new_file = Arg.conv (Fpath.of_string, Fpath.pp)

let output =
  let doc = "The path of the produced email with the new Received-SPF field." in
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

let sender =
  let parser str =
    match R.(Emile.of_string str >>= Colombe_emile.to_path) with
    | Ok v -> Ok v
    | Error _ -> R.error_msgf "Invalid sender: %S" str in
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

let stamp =
  let doc =
    "Stamps the given message with the Received-SPF field and its result." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Stamps the given email with a new Received-SPF field and its result \
         from given arguments (such as the $(i,ip) address and the \
         $(i,sender)).";
    ] in
  let open Term in
  let info = Cmd.info "stamp" ~doc ~man in
  let term =
    const stamp
    $ setup_logs
    $ hostname
    $ setup_resolver
    $ sender
    $ helo
    $ ip
    $ input
    $ output in
  Cmd.v info (ret term)

let analyze =
  let doc =
    "Analyzes Received-SPF fields from the given message and shows their \
     results and the reproductibility of these results." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Analyzes the given email and extract Received-SPF fields to reproduce \
         expected results.";
    ] in
  let open Term in
  let info = Cmd.info "analyze" ~doc ~man in
  let term = const analyze $ setup_logs $ setup_resolver $ input in
  Cmd.v info (ret term)

let default = Term.(ret (const (`Help (`Pager, None))))

let cmd =
  let doc = "A tool to manipulate Received-SPF fields." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Use $(tname) $(i,stamp) to stamp the incoming email with an \
         Received-SPF result.";
      `P
        "Use $(tname) $(i,analyze) to check Received-SPF fields from the \
         incoming email.";
    ] in
  Cmd.group ~default (Cmd.info "spf" ~doc ~man) [ stamp; analyze ]
