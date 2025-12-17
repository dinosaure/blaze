open Cmdliner

let src = Logs.Src.create "blaze.args"

module Log = (val Logs.src_log src : Logs.LOG)

let () = Logs_threaded.enable ()
let ( % ) f g x = f (g x)
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

(* Basics and unqualified [cmdliner] values. *)

let existing_directory =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.is_directory str -> Ok (Fpath.to_dir_path v)
    | Ok v ->
        error_msgf "%a does not exist or it's not a valid directory" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let non_existing_file =
  let parser str =
    let normalized = Fpath.of_string str in
    if Result.is_ok normalized && Sys.file_exists str = false
    then Ok str
    else error_msgf "Invalid path: %S" str in
  Arg.conv (parser, Fmt.string)

let domain_name = Arg.conv (Domain_name.of_string, Domain_name.pp)

(* Temporary files. *)

let tmp =
  let doc = "The directory to keep temporary files produced by $(tname)." in
  let env = Cmd.Env.info "BLAZE_TMP" in
  let open Arg in
  value
  & opt existing_directory (Fpath.v "/tmp")
  & info [ "tmp" ] ~env ~doc ~docv:"DIRECTORY"

let setup_tmp tmp = Blaze_tmp.set_temp_dirname (Fpath.to_string tmp)
let setup_tmp = Term.(const setup_tmp $ tmp)

(* Logs options (see [logs]). *)

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
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
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

let file =
  let parser str =
    if str = "-"
    then Ok str
    else if Sys.file_exists str && Sys.is_directory str = false
    then Ok str
    else error_msgf "%s does not exist" str in
  Arg.conv (parser, Fmt.string)

(* Happy-eyeballs options (see [happy-eyeballs]). *)

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

(* Nameservers (for DNS) options. *)

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

(* Domains option. *)

let default_threads = Int.min 4 (Stdlib.Domain.recommended_domain_count () - 1)

let threads ?(min = default_threads) () =
  let doc = "The number of threads to allocate for the computation." in
  let open Arg in
  value & opt int min & info [ "t"; "threads" ] ~doc ~docv:"NUMBER"

(* DNS options (see [ldns]). *)

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

let setup_resolver happy_eyeballs_cfg dns_cfg nameservers local () =
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

(* Random option. *)

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
  let pp = Fmt.using (Base64.encode_exn % int_array_to_string) Fmt.string in
  Arg.conv ~docv:"<seed>" (parser, pp)

let seed =
  let doc =
    "The seed (in base64) used to initialize the random number generator." in
  Arg.(value & opt (some seed) None & info [ "s"; "seed" ] ~doc ~docv:"<seed>")

let setup_random = function
  | None -> Random.State.make_self_init ()
  | Some seed -> Random.State.make seed

let setup_random = Term.(const setup_random $ seed)

(** Progress options (see [progress]). *)

let setup_progress max_width = Progress.Config.v ~max_width ()

let width =
  let doc = "Width of the terminal." in
  let default = Terminal.Size.get_columns () in
  let open Arg in
  value & opt (some int) default & info [ "width" ] ~doc ~docv:"WIDTH"

let setup_progress = Term.(const setup_progress $ width)

let without_progress =
  let doc = "Don't print progress bar." in
  Arg.(value & flag & info [ "without-progress" ] ~doc)

(** Newline option. *)

let newline_options = "EMAILS AND NEWLINE"

let newline ?(default = `LF) () =
  let parser str =
    match String.lowercase_ascii str with
    | "crlf" -> Ok `CRLF
    | "lf" -> Ok `LF
    | _ -> error_msgf "Invalid newline" in
  let pp ppf = function
    | `CRLF -> Fmt.string ppf "crlf"
    | `LF -> Fmt.string ppf "lf" in
  let newline = Arg.conv (parser, pp) in
  let doc = "The newline used by emails." in
  let open Arg in
  value
  & opt newline default
  & info [ "newline" ] ~doc ~docs:newline_options ~docv:"NEWLINE"

(** Crypto keys options (see [mirage-crypto]). *)

type key =
  [ `RSA of Mirage_crypto_pk.Rsa.priv
  | `ED25519 of Mirage_crypto_ec.Ed25519.priv ]

let private_key : key Arg.conv =
  let parser str =
    let ( let* ) = Result.bind in
    let key =
      let* key = Base64.decode ~pad:true str in
      match X509.Private_key.decode_der key with
      | Ok #key as key -> key
      | Ok _ -> error_msgf "Invalid algorithm used for DKIM signature"
      | Error _ as err -> err in
    match (key, Fpath.of_string str) with
    | (Ok _ as v), _ -> v
    | Error _, Ok filename
      when Sys.file_exists str && not (Sys.is_directory str) ->
        let ic = open_in (Fpath.to_string filename) in
        let len = in_channel_length ic in
        let buf = Bytes.create len in
        really_input ic buf 0 len ;
        close_in ic ;
        let str = Bytes.unsafe_to_string buf in
        begin match X509.Private_key.decode_pem str with
        | Ok #key as key -> key
        | Ok _ -> error_msgf "Invalid algorithm used for DKIM signature"
        | Error _ as err -> err
        end
    | (Error _ as err), _ -> err in
  let pp ppf (pk : key) =
    Fmt.string ppf (X509.Private_key.encode_der (pk :> X509.Private_key.t))
  in
  Arg.conv (parser, pp)

let hash =
  let parser str =
    match String.(trim (lowercase_ascii str)) with
    | "sha1" -> Ok `SHA1
    | "sha256" -> Ok `SHA256
    | _ -> error_msgf "Invalid hash: %S" str in
  let pp ppf = function
    | `SHA1 -> Fmt.string ppf "sha1"
    | `SHA256 -> Fmt.string ppf "sha256" in
  Arg.conv (parser, pp)

let algorithm =
  let parser str =
    match String.trim (String.lowercase_ascii str) with
    | "rsa" -> Ok `RSA
    | "ed25519" -> Ok `ED25519
    | _ -> error_msgf "Invalid algorithm: %S" str in
  let pp ppf = function
    | `RSA -> Fmt.string ppf "rsa"
    | `ED25519 -> Fmt.string ppf "ed25519" in
  Arg.conv (parser, pp)

let pot x = x land (x - 1) == 0 && x != 0

let bits =
  let parser str =
    try
      let v = int_of_string str in
      if pot v then Ok v else error_msgf "The given value is not a power of two"
    with _ -> error_msgf "Invalid number" in
  Arg.conv (parser, Fmt.int)

let base64 =
  let parser str = Base64.decode ~pad:true str in
  let pp ppf seed = Fmt.string ppf (Base64.encode_exn ~pad:true seed) in
  Arg.conv (parser, pp)

(* Canonicalization options (see [dkim]). *)

let canon =
  let parser str =
    let v = String.trim str in
    let v = String.lowercase_ascii v in
    match String.split_on_char '/' v with
    | [ "simple"; "simple" ] | [] | [ "simple" ] -> Ok (`Simple, `Simple)
    | [ "simple"; "relaxed" ] -> Ok (`Simple, `Relaxed)
    | [ "relaxed"; "simple" ] -> Ok (`Relaxed, `Simple)
    | [ "relaxed"; "relaxed" ] | [ "relaxed" ] -> Ok (`Relaxed, `Relaxed)
    | _ -> error_msgf "Invalid canonicalization specification: %S" str in
  let pp ppf = function
    | `Simple, `Simple -> Fmt.string ppf "simple"
    | `Relaxed, `Relaxed -> Fmt.string ppf "relaxed"
    | `Simple, `Relaxed -> Fmt.string ppf "simple/relaxed"
    | `Relaxed, `Simple -> Fmt.string ppf "relaxed/simple" in
  Arg.conv (parser, pp)

(* Hexdump options (see [hxd]). *)

let docs_hexdump = "HEX OUTPUT"

let colorscheme =
  let x = Array.make 256 `None in
  for i = 0 to 31 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xd7, 0xff))
  done ;
  for i = 48 to 57 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xdf, 0x77))
  done ;
  for i = 65 to 90 do
    x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0x5f))
  done ;
  for i = 97 to 122 do
    x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0xd7))
  done ;
  Hxd.colorscheme_of_array x

let cols =
  let doc = "Format $(i,COLS) octets per line. Default 16. Max 256." in
  let parser str =
    match int_of_string str with
    | n when n < 1 || n > 256 ->
        error_msgf "Invalid COLS value (must <= 256 && > 0): %d" n
    | n -> Ok n
    | exception _ -> error_msgf "Invalid COLS value: %S" str in
  let open Arg in
  let cols = conv (parser, Fmt.int) in
  value
  & opt (some cols) None
  & info [ "c"; "cols" ] ~doc ~docv:"COLS" ~docs:docs_hexdump

let groupsize =
  let doc =
    "Separate the output of every $(i,bytes) bytes (two hex characters) by a \
     whitespace. Specify -g 0 to supress grouping. $(i,bytes) defaults to 2."
  in
  let open Arg in
  value
  & opt (some int) None
  & info [ "g"; "groupsize" ] ~doc ~docv:"BYTES" ~docs:docs_hexdump

let len =
  let doc = "Stop after writing $(i,LEN) octets." in
  let open Arg in
  value
  & opt (some int) None
  & info [ "l"; "len" ] ~doc ~docv:"LEN" ~docs:docs_hexdump

let uppercase =
  let doc = "Use upper case hex letters. Default is lower case." in
  let open Arg in
  value & flag & info [ "u" ] ~doc ~docs:docs_hexdump

let setup_hxd cols groupsize len uppercase =
  Hxd.xxd ?cols ?groupsize ?long:len ~uppercase colorscheme

let setup_hxd = Term.(const setup_hxd $ cols $ groupsize $ len $ uppercase)

(* Languages and tokenizers options (see [stem]). *)

let language_from_string str =
  let algs = Snowball.languages in
  let str = String.lowercase_ascii str in
  let fn (alg : Snowball.Language.t) = String.equal str (alg :> string) in
  match List.find_opt fn algs with
  | Some alg -> Ok alg
  | None -> error_msgf "Language %S not found" str

let tokenizer_from_string str =
  match String.lowercase_ascii str with
  | "whitespace" -> Ok Tokenizer.Whitespace
  | "dash" -> Ok Dash
  | "bert" -> Ok Bert
  | _ -> error_msgf "Invalid tokenizer: %S" str

let behavior_from_string str =
  match String.lowercase_ascii str with
  | "remove" -> Ok Tokenizer.Remove
  | "isolate" -> Ok Isolate
  | "merge_with_previous" | "merge-with-previous" -> Ok Merge_with_previous
  | "merge_with_next" | "merge-with-next" -> Ok Merge_with_next
  | _ -> error_msgf "Invalid behavior: %S" str

let re_from_string str =
  try Ok (Re.Pcre.regexp str)
  with _ -> error_msgf "Invalid regular expression: %S" str

let pp_tokenizer ppf = function
  | Tokenizer.Whitespace -> Fmt.string ppf "whitespace"
  | Dash -> Fmt.string ppf "dash"
  | Bert -> Fmt.string ppf "bert"
  | Regex re -> Fmt.pf ppf "re:%a" Re.pp_re re

let pp_behavior ppf = function
  | Tokenizer.Remove -> Fmt.string ppf "remove"
  | Isolate -> Fmt.string ppf "isolate"
  | Merge_with_previous -> Fmt.string ppf "merge-with-previous"
  | Merge_with_next -> Fmt.string ppf "merge-with-next"

let language =
  let algs = Snowball.languages in
  let pp ppf (alg : Snowball.Language.t) = Fmt.string ppf (alg :> string) in
  let language = Arg.conv (language_from_string, pp) in
  let doc =
    let algs =
      List.map (fun (alg : Snowball.Language.t) -> (alg :> string)) algs in
    let hd, tl = (List.hd algs, List.tl algs) in
    let tl = List.rev tl in
    Fmt.str
      "The language to process. $(tname) is able to handle these languages: %s \
       and %s."
      (String.concat ", " tl) hd in
  let open Arg in
  value
  & opt language Snowball.porter
  & info [ "l"; "language" ] ~doc ~docv:"LANGUAGE"

let encoding =
  let open Arg in
  let docs = "ENCODINGS" in
  let encodings =
    [
      (Snowball.UTF_8, info [ "utf-8" ] ~doc:"UTF-8 encoding" ~docs);
      ( Snowball.ISO_8859_1,
        info [ "iso-8859-1"; "latin1" ] ~doc:"Latin1 encoding" ~docs );
      ( Snowball.ISO_8859_2,
        info [ "iso-8859-2"; "latin2" ] ~doc:"Latin2 encoding" ~docs );
      (Snowball.KOI8_R, info [ "koi8-r" ] ~doc:"KOI8-R encoding" ~docs);
    ] in
  value & vflag Snowball.UTF_8 encodings

let action =
  let ( let* ) = Result.bind in
  let parser str =
    match String.split_on_char ':' str with
    | [ tokenizer ] ->
        let* tokenizer = tokenizer_from_string tokenizer in
        Ok (tokenizer, Tokenizer.Remove)
    | ("re" | "RE" | "rE" | "Re") :: behavior :: re ->
        let* behavior = behavior_from_string behavior in
        let* re = re_from_string (String.concat ":" re) in
        Ok (Tokenizer.Regex re, behavior)
    | tokenizer :: behavior ->
        let behavior = String.concat ":" behavior in
        let* tokenizer = tokenizer_from_string tokenizer in
        let* behavior = behavior_from_string behavior in
        Ok (tokenizer, behavior)
    | [] -> assert false in
  let pp ppf (tokenizer, action) =
    match tokenizer with
    | Tokenizer.Regex re -> Fmt.pf ppf "re:%a:%a" pp_behavior action Re.pp_re re
    | tokenizer -> Fmt.pf ppf "%a:%a" pp_tokenizer tokenizer pp_behavior action
  in
  Arg.conv (parser, pp)

let actions =
  let doc = "An action to $(i,tokenize) the given document and split words." in
  let open Arg in
  value
  & opt_all action Tokenizer.[ (Whitespace, Remove); (Bert, Remove) ]
  & info [ "a"; "action" ] ~doc ~docv:"ACTION"
