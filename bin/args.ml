open Cmdliner

let src = Logs.Src.create "blaze.args"

module Log = (val Logs.src_log src : Logs.LOG)

let ( <.> ) f g x = f (g x)
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let output_options = "OUTPUT OPTIONS"

let verbosity =
  let env = Cmd.Env.info "BLAZE_LOGS" in
  Logs_cli.level ~docs:output_options ~env ()

let renderer =
  let env = Cmd.Env.info "BLAZE_FMT" in
  Fmt_cli.style_renderer ~docs:output_options ~env ()

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
let docs_dns = "DOMAIN NAME SERVICE"

let timeout =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let parser str =
    let len =
      let len = ref 0 in
      while !len < String.length str && is_digit str.[!len] do
        incr len
      done ;
      !len in
    let meter = String.sub str len (String.length str - len) in
    let value = String.sub str 0 len in
    match meter with
    | "ns" -> Ok (Int64.of_string value)
    | "us" -> Ok (Duration.of_us (int_of_string value))
    | "ms" -> Ok (Duration.of_ms (int_of_string value))
    | "sec" | "s" -> Ok (Duration.of_sec (int_of_string value))
    | "min" | "m" -> Ok (Duration.of_min (int_of_string value))
    | "hour" | "h" -> Ok (Duration.of_hour (int_of_string value))
    | _ -> error_msgf "Invalid time: %S" str in
  Arg.conv ~docv:"TIME" (parser, Duration.pp)

let aaaa_timeout =
  let doc = "The timeout applied to the IPv6 resolution." in
  let open Arg in
  value
  & opt (some timeout) None
  & info [ "aaaa-timeout" ] ~doc ~docv:"TIME" ~docs:docs_dns

let connect_delay =
  let doc =
    "Time to repeat another connection attempt if the others don't respond."
  in
  let open Arg in
  value
  & opt (some timeout) None
  & info [ "connect-delay" ] ~doc ~docv:"TIME" ~docs:docs_dns

let connect_timeout =
  let doc = "The timeout applied to $(b,connect())." in
  let open Arg in
  value
  & opt (some timeout) None
  & info [ "connect-timeout" ] ~doc ~docv:"TIME" ~docs:docs_dns

let resolve_timeout =
  let doc = "The timeout applied to the domain-name resolution." in
  let open Arg in
  value
  & opt (some timeout) None
  & info [ "resolve-timeout" ] ~doc ~docv:"TIME" ~docs:docs_dns

let resolve_retries =
  let doc = "The number $(i,N) of attempts to make a connection." in
  let open Arg in
  value
  & opt (some int) None
  & info [ "resolve-retries" ] ~doc ~docv:"N" ~docs:docs_dns

type happy_eyeballs = {
  aaaa_timeout : int64 option;
  connect_delay : int64 option;
  connect_timeout : int64 option;
  resolve_timeout : int64 option;
  resolve_retries : int option;
}

let setup_happy_eyeballs aaaa_timeout connect_delay connect_timeout
    resolve_timeout resolve_retries = function
  | false ->
      Some
        {
          aaaa_timeout;
          connect_delay;
          connect_timeout;
          resolve_timeout;
          resolve_retries;
        }
  | _ -> None

let without_happy_eyeballs =
  let doc = "Don't use the happy-eyeballs algorithm (RFC8305)." in
  let open Arg in
  value & flag & info [ "without-happy-eyeballs" ] ~doc ~docs:docs_dns

let setup_happy_eyeballs =
  let open Term in
  const setup_happy_eyeballs
  $ aaaa_timeout
  $ connect_delay
  $ connect_timeout
  $ resolve_timeout
  $ resolve_retries
  $ without_happy_eyeballs

let nameserver_of_string str =
  let ( let* ) = Result.bind in
  match String.split_on_char ':' str with
  | "tls" :: rest -> (
      let str = String.concat ":" rest in
      match String.split_on_char '!' str with
      | [ nameserver ] ->
          let* ipaddr, port =
            Ipaddr.with_port_of_string ~default:853 nameserver in
          let* authenticator = Ca_certs.authenticator () in
          let* tls = Tls.Config.client ~authenticator () in
          Ok (`Tcp, `Tls (tls, ipaddr, port))
      | nameserver :: authenticator ->
          let* ipaddr, port =
            Ipaddr.with_port_of_string ~default:853 nameserver in
          let authenticator = String.concat "!" authenticator in
          let* authenticator = X509.Authenticator.of_string authenticator in
          let time () = Some (Ptime.v (Ptime_clock.now_d_ps ())) in
          let authenticator = authenticator time in
          let* tls = Tls.Config.client ~authenticator () in
          Ok (`Tcp, `Tls (tls, ipaddr, port))
      | [] -> assert false)
  | "tcp" :: nameserver ->
      let str = String.concat ":" nameserver in
      let* ipaddr, port = Ipaddr.with_port_of_string ~default:53 str in
      Ok (`Tcp, `Plaintext (ipaddr, port))
  | "udp" :: nameserver | nameserver ->
      let str = String.concat ":" nameserver in
      let* ipaddr, port = Ipaddr.with_port_of_string ~default:53 str in
      Ok (`Udp, `Plaintext (ipaddr, port))

type nameserver =
  [ `Plaintext of Ipaddr.t * int | `Tls of Tls.Config.client * Ipaddr.t * int ]

let nameserver =
  let parser = nameserver_of_string in
  let pp ppf = function
    | _, `Tls (_, ipaddr, 853) ->
        Fmt.pf ppf "tls:%a!<authenticator>" Ipaddr.pp ipaddr
    | _, `Tls (_, ipaddr, port) ->
        Fmt.pf ppf "tls:%a:%d!<authenticator>" Ipaddr.pp ipaddr port
    | `Tcp, `Plaintext (ipaddr, 53) -> Fmt.pf ppf "tcp:%a" Ipaddr.pp ipaddr
    | `Udp, `Plaintext (ipaddr, 53) -> Fmt.pf ppf "%a" Ipaddr.pp ipaddr
    | `Tcp, `Plaintext (ipaddr, port) ->
        Fmt.pf ppf "tcp:%a:%d" Ipaddr.pp ipaddr port
    | `Udp, `Plaintext (ipaddr, port) ->
        Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port in
  Arg.conv (parser, pp) ~docv:"NAMESERVER"

let nameservers =
  let doc = "The $(i,NAMESERVER) used to resolve domain-names." in
  let google_com = (`Udp, `Plaintext (Ipaddr.of_string_exn "8.8.8.8", 53)) in
  let open Arg in
  value
  & opt_all nameserver [ google_com ]
  & info [ "n"; "nameserver" ] ~doc ~docv:"NAMESERVER" ~docs:docs_dns

let setup_nameservers nameservers =
  let tcp, udp =
    List.partition_map
      (function `Udp, v -> Either.Right v | `Tcp, v -> Either.Left v)
      nameservers in
  match (tcp, udp) with
  | [], nameservers -> `Ok (`Udp, nameservers)
  | nameservers, [] -> `Ok (`Tcp, nameservers)
  | _ -> `Error (true, "Impossible to use TCP & UDP protocols for nameservers")

let setup_nameservers = Term.(ret (const setup_nameservers $ nameservers))

let dns =
  let open Arg in
  let system =
    let doc =
      "Domain name resolution is done by the system (usually 127.0.0.53:53)."
    in
    info [ "system" ] ~doc ~docs:docs_dns in
  let internal =
    let doc =
      "Domain name resolution is done by the program itself with given \
       nameservers." in
    info [ "internal" ] ~doc ~docs:docs_dns in
  value & vflag `System [ (`System, system); (`Internal, internal) ]

let existing_directory =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.is_directory str -> Ok v
    | Ok v ->
        error_msgf "%a does not exist or it's not a valid directory" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let dns_static =
  let env = Cmd.Env.info "BLAZE_DNS_STATIC" in
  let doc = "Load a static DNS tree." in
  Arg.(
    value & opt (some existing_directory) None & info [ "dns-static" ] ~env ~doc)

let setup_dns_static = function
  | None -> Domain_name.Map.empty
  | Some fpath ->
  match Dns_static.of_directory fpath with
  | Ok local -> local
  | Error (`Msg err) ->
      Log.err (fun m ->
          m "Got an error when we loaded the local DNS: %a: %s" Fpath.pp fpath
            err) ;
      Domain_name.Map.empty

let setup_dns_static = Term.(const setup_dns_static $ dns_static)

let setup_resolver happy_eyeballs_cfg dns_cfg nameservers local =
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
  match dns_cfg with
  | `System ->
      let daemon, he = Happy_eyeballs_miou_unix.create ?happy_eyeballs () in
      (daemon, he)
  | `Internal ->
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
      (daemon, he)

let setup_resolver =
  let open Term in
  const setup_resolver
  $ setup_happy_eyeballs
  $ dns
  $ setup_nameservers
  $ setup_dns_static

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
