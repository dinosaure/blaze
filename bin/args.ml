open Rresult
open Cmdliner

let src = Logs.Src.create "blaze.args"

module Log = (val Logs.src_log src : Logs.LOG)

let ( <.> ) f g x = f (g x)
let common_options = "COMMON OPTIONS"

let verbosity =
  let env = Cmd.Env.info "BLAZE_LOGS" in
  Logs_cli.level ~docs:common_options ~env ()

let renderer =
  let env = Cmd.Env.info "BLAZE_FMT" in
  Fmt_cli.style_renderer ~docs:common_options ~env ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  let env = Cmd.Env.info "BLAZE_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

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

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr) ;
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let inet_addr_of_string str =
  match Unix.inet_addr_of_string str with v -> Some v | exception _ -> None

let pp_nameserver ppf = function
  | `Tcp, (inet_addr, 53) :: _ -> Fmt.pf ppf "tcp://%a/" Ipaddr.pp inet_addr
  | `Udp, (inet_addr, 53) :: _ -> Fmt.pf ppf "udp://%a/" Ipaddr.pp inet_addr
  | `Tcp, (inet_addr, port) :: _ ->
      Fmt.pf ppf "tcp://%a:%d/" Ipaddr.pp inet_addr port
  | `Udp, (inet_addr, port) :: _ ->
      Fmt.pf ppf "udp://%a:%d/" Ipaddr.pp inet_addr port
  | `Tcp, [] -> Fmt.pf ppf "tcp:///"
  | `Udp, [] -> Fmt.pf ppf "udp:///"

let nameserver =
  let parser str =
    let uri = Uri.of_string str in
    let via =
      match Uri.scheme uri with
      | None | Some "udp" -> `Udp
      | Some "tcp" -> `Tcp
      | Some scheme -> Fmt.invalid_arg "Invalid scheme: %S" scheme in
    match (Option.bind (Uri.host uri) inet_addr_of_string, Uri.port uri) with
    | None, None -> None
    | None, Some port ->
        Some (via, [ (Ipaddr_unix.of_inet_addr Unix.inet_addr_loopback, port) ])
    | Some inet_addr, None ->
        Some (via, [ (Ipaddr_unix.of_inet_addr inet_addr, 53) ])
    | Some inet_addr, Some port ->
        Some (via, [ (Ipaddr_unix.of_inet_addr inet_addr, port) ]) in
  let parser str =
    match parser str with
    | Some v -> Ok v
    | None -> R.error_msgf "Invalid nameserver: %a" Uri.pp (Uri.of_string str)
    | exception _ -> R.error_msgf "Invalid nameserver: %S" str in
  Arg.conv (parser, pp_nameserver)

let nameserver =
  let doc = "DNS nameserver." in
  Arg.(value & opt (some nameserver) None & info [ "n"; "nameserver" ] ~doc)

let existing_directory =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.is_directory str -> Ok v
    | Ok v ->
        R.error_msgf "%a does not exist or it's not a valid directory" Fpath.pp
          v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let local_dns =
  let env = Cmd.Env.info "BLAZE_DNS" in
  let doc = "Load a local DNS cache." in
  Arg.(value & opt (some existing_directory) None & info [ "dns" ] ~env ~doc)

let setup_local_dns = function
  | None -> Domain_name.Map.empty
  | Some fpath ->
  match Ldns.of_directory fpath with
  | Ok local -> local
  | Error (`Msg err) ->
      Log.err (fun m ->
          m "Got an error when we loaded the local DNS: %a: %s" Fpath.pp fpath
            err) ;
      Domain_name.Map.empty

let setup_local_dns = Term.(const setup_local_dns $ local_dns)

let timeout =
  let doc = "The DNS timeout allowed (in nano-second)." in
  Arg.(value & opt int64 5_000_000_000L & info [ "t"; "timeout" ] ~doc)

let string_to_int_array str =
  let res = Array.make (String.length str / 2) 0 in
  for i = 0 to (String.length str / 2) - 1 do
    res.(i) <- (Char.code str.[i * 2] lsl 8) lor Char.code str.[(i * 2) + 1]
  done ;
  res

let int_array_to_string arr =
  let buf = Bytes.create (Array.length arr * 2) in
  for i = 0 to Array.length arr - 1 do
    Bytes.set buf (2 * i) (Char.unsafe_chr (arr.(i) lsr 8)) ;
    Bytes.set buf ((2 * i) + 1) (Char.unsafe_chr arr.(i))
  done ;
  Bytes.unsafe_to_string buf

let seed =
  let parser str =
    match Base64.decode str with
    | Ok seed -> Ok (string_to_int_array seed)
    | Error _ as err -> err in
  let pp = Fmt.using (Base64.encode_exn <.> int_array_to_string) Fmt.string in
  Arg.conv ~docv:"<seed>" (parser, pp)

let seed =
  let doc =
    "The seed (in base64) used to initialize the random number generator." in
  Arg.(value & opt (some seed) None & info [ "s"; "seed" ] ~doc ~docv:"<seed>")

let setup_random = function
  | None -> Random.State.make_self_init ()
  | Some seed -> Random.State.make seed

let setup_random = Term.(const setup_random $ seed)
