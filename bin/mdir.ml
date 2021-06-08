open Rresult

let rec transmit ic oc =
  let tmp = Bytes.create 0x1000 in
  go tmp ic oc

and go tmp ic oc =
  let len = input ic tmp 0 (Bytes.length tmp) in
  if len > 0
  then (
    output oc tmp 0 len ;
    go tmp ic oc)

let () = Random.self_init () (* TODO(dinosaure): user-specified.  *)

let random () = Random.int64 Int64.max_int

let get _ hostname maildir new_message message output =
  let message = if new_message then Maildir.with_new message else message in
  let host = Domain_name.to_string hostname in
  if message.Maildir.value.Maildir.host <> host
  then
    Logs.warn (fun m ->
        m "The given host (%s) is different from the host's message (%s)." host
          message.Maildir.value.Maildir.host) ;
  let maildir = Maildir.create ~pid:(Unix.getpid ()) ~host ~random maildir in
  let fpath = Maildir_unix.get maildir message in
  if Sys.file_exists (Fpath.to_string fpath)
  then (
    let ic = open_in (Fpath.to_string fpath) in
    let oc, close_oc =
      match output with
      | Some fpath -> (open_out (Fpath.to_string fpath), close_out)
      | None -> (stdout, ignore) in
    transmit ic oc ;
    close_in ic ;
    close_oc oc ;
    `Ok 0)
  else `Error (false, Fmt.str "%a does not exist." Fpath.pp fpath)

let new_messages _ hostname maildir =
  let host = Domain_name.to_string hostname in
  let maildir = Maildir.create ~pid:(Unix.getpid ()) ~host ~random maildir in
  let fold () { Maildir.value; _ } = Fmt.pr "%a\n%!" Maildir.pp_message value in
  Maildir_unix.scan_only_new fold () Maildir_unix.fs maildir ;
  `Ok 0

open Cmdliner

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

let maildir =
  let parser str =
    match Fpath.of_string str with
    | Ok _ as v when Sys.is_directory str -> v
    | Ok v -> R.error_msgf "%a is not an existing directory" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let maildir =
  let doc = "The $(i,maildir) path." in
  let env = Arg.env_var ~doc "BLAZE_MDIR" in
  Arg.(required & opt (some maildir) None & info [ "D"; "maildir" ] ~env ~doc)

let message =
  let parser str = Maildir.of_filename str in
  let pp ppf { Maildir.value; _ } = Maildir.pp_message ppf value in
  Arg.conv (parser, pp)

let message =
  let doc = "The message identifier." in
  Arg.(required & pos ~rev:true 0 (some message) None & info [] ~doc)

let new_file = Arg.conv (Fpath.of_string, Fpath.pp)

let output =
  let doc = "The path of the file to store the given email." in
  Arg.(value & opt (some new_file) None & info [ "o"; "output" ] ~doc)

let domain_name = Arg.conv (Domain_name.of_string, Domain_name.pp)

let hostname =
  let doc = "The hostname of the computer." in
  Arg.(
    value
    & opt domain_name (Domain_name.of_string_exn (Unix.gethostname ()))
    & info [ "h"; "hostname" ] ~doc)

let new_message =
  let doc = "If the message is a new one." in
  Arg.(value & flag & info [ "n"; "new" ] ~doc)

let seed =
  let doc = "Seed used by the random number generator." in
  let base64 =
    Arg.conv
      ((fun str -> Base64.decode str), Fmt.using Base64.encode_string Fmt.string)
  in
  Arg.(value & opt (some base64) None & info [ "s"; "seed" ] ~doc)

let get =
  let doc = "Load and store the given message to the $(i,output)." in
  let man = [] in
  ( Term.(
      ret
        (const get
        $ setup_logs
        $ hostname
        $ maildir
        $ new_message
        $ message
        $ output)),
    Term.info "get" ~doc ~man )

let new_messages =
  let doc = "Scan and show new messages from the given $(i,maildir)." in
  let man =
    [
      `S Manpage.s_description;
      `P "From the given $(i,maildir), $(b,new) shows new messages.";
    ] in
  ( Term.(ret (const new_messages $ setup_logs $ hostname $ maildir)),
    Term.info "new" ~doc ~man )

let default =
  let doc = "A tool to manipulate a $(i,maildir) directory." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "From the given $(i,maildir) and the message $(i,id), $(b,get) loads \
         and shows the entire message.";
    ] in
  (Term.(ret (const (`Help (`Pager, None)))), Term.info "mdir" ~doc ~man)

let () = Term.(exit_status @@ eval_choice default [ get; new_messages ])
