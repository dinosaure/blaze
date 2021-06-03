open Rresult

let ctx sender helo ip =
  Spf.empty |> fun ctx ->
  Option.fold ~none:ctx
    ~some:(fun helo -> Spf.with_sender (`HELO helo) ctx)
    helo
  |> fun ctx ->
  Option.fold ~none:ctx
    ~some:(fun sender -> Spf.with_sender (`MAILFROM sender) ctx)
    sender
  |> fun ctx -> Option.fold ~none:ctx ~some:(fun ip -> Spf.with_ip ip ctx) ip

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

let stamp quiet nameserver timeout hostname sender helo ip input _output =
  let ic, close_ic =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let ctx = ctx sender helo ip in
  match Spf_unix.check ?nameserver ~timeout ctx with
  | Ok res when quiet -> (
      match res with
      | `Pass _ | `None | `Neutral -> `Ok 0
      | `Fail | `Softfail | `Permerror | `Temperror -> `Ok 1)
  | Ok res ->
      let field_name, unstrctrd = Spf.to_field ~ctx ~receiver:hostname res in
      Fmt.pr "%a: %s\n%!" Mrmime.Field_name.pp field_name
        (unstrctrd_to_utf_8_string_with_lf unstrctrd) ;
      transmit ic stdout ;
      close_ic ic ;
      `Ok 0
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

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
  if !res then `Ok 0 else `Ok 1

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
  `Ok 0

let analyze quiet nameserver timeout input =
  let ic, close_ic =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let res = Spf_unix.extract_received_spf ~newline:Spf.LF ic in
  close_ic ic ;
  match res with
  | Ok extracted ->
      let results =
        List.fold_left
          (fun acc { Spf.result; ctx; sender; ip; _ } ->
            match Spf_unix.check ?nameserver ~timeout ctx with
            | Ok v -> (sender, ip, result, v) :: acc
            | _ -> acc)
          [] extracted in
      if quiet then to_exit_code results else show_results results
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

open Cmdliner

let inet_addr_of_string str =
  match Unix.inet_addr_of_string str with v -> Some v | exception _ -> None

let pp_nameserver ppf = function
  | `TCP, (inet_addr, 53) -> Fmt.pf ppf "tcp://%a/" Ipaddr.pp inet_addr
  | `UDP, (inet_addr, 53) -> Fmt.pf ppf "udp://%a/" Ipaddr.pp inet_addr
  | `TCP, (inet_addr, port) ->
      Fmt.pf ppf "tcp://%a:%d/" Ipaddr.pp inet_addr port
  | `UDP, (inet_addr, port) ->
      Fmt.pf ppf "udp://%a:%d/" Ipaddr.pp inet_addr port

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
    | None, Some port ->
        Some (via, (Ipaddr_unix.of_inet_addr Unix.inet_addr_loopback, port))
    | Some inet_addr, None ->
        Some (via, (Ipaddr_unix.of_inet_addr inet_addr, 53))
    | Some inet_addr, Some port ->
        Some (via, (Ipaddr_unix.of_inet_addr inet_addr, port)) in
  let parser str =
    match parser str with
    | Some v -> Ok v
    | None -> R.error_msgf "Invalid nameserver: %a" Uri.pp (Uri.of_string str)
    | exception _ -> R.error_msgf "Invalid nameserver: %S" str in
  Arg.conv (parser, pp_nameserver)

let nameserver =
  let doc = "DNS nameserver." in
  Arg.(value & opt (some nameserver) None & info [ "n"; "nameserver" ] ~doc)

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
  let doc = "The email to check." in
  Arg.(value & pos 0 existing_file None & info [] ~doc)

let new_file = Arg.conv (Fpath.of_string, Fpath.pp)

let output =
  let doc = "The path of the produced email with the new Received-SPF field." in
  Arg.(value & opt (some new_file) None & info [ "o"; "output" ] ~doc)

let timeout =
  let doc = "The DNS timeout allowed (in nano-second)." in
  Arg.(value & opt int64 5_000_000_000L & info [ "t"; "timeout" ] ~doc)

let domain =
  let parser str =
    match Angstrom.parse_string ~consume:All Emile.Parser.domain str with
    | Ok v -> Ok v
    | Error _ -> R.error_msgf "Invalid domain: %S" str in
  let pp = Emile.pp_domain in
  Arg.conv (parser, pp)

let domain =
  let doc = "The hostname of the computer." in
  Arg.(
    value
    & opt domain (R.get_ok (Arg.conv_parser domain (Unix.gethostname ())))
    & info [ "h"; "hostname" ] ~doc)

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
  let doc = "" in
  let man = [] in
  ( Term.(
      ret
        (const stamp
        $ setup_logs
        $ nameserver
        $ timeout
        $ domain
        $ sender
        $ helo
        $ ip
        $ input
        $ output)),
    Term.info "stamp" ~doc ~man )

let analyze =
  let doc = "" in
  let man = [] in
  ( Term.(ret (const analyze $ setup_logs $ nameserver $ timeout $ input)),
    Term.info "analyze" ~doc ~man )

let default =
  let doc = "A tool to manipulate Received-SPF fields." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Use $(tname) $(i,stamp) to stamp the incoming email with an \
         Received-SPF\n\
        \        result.";
      `P
        "Use $(tname) $(i,analyze) to check Received-SPF fields from the \
         incoming\n\
        \        email.";
    ] in
  (Term.(ret (const (`Help (`Pager, None)))), Term.info "spf" ~doc ~man)

let () = Term.(exit_status @@ eval_choice default [ stamp; analyze ])
