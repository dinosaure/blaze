let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let reword_error fn = function Ok _ as v -> v | Error err -> Error (fn err)

type uid = [ `POP3 of string ]
type protocol = [ `POP3 | `Git ]

type remote =
  | Uri of protocol * (string * string) option * string * int option * string
  | Git of string * string * string

let decode_host_port str =
  match String.split_on_char ':' str with
  | [] -> assert false
  | [ host ] -> Ok (host, None)
  | [ host; "" ] -> Ok (host, None)
  | hd :: tl -> (
      let port, host =
        match List.rev (hd :: tl) with
        | hd :: tl -> (hd, String.concat ":" (List.rev tl))
        | _ -> assert false in
      try Ok (host, Some (int_of_string port))
      with _ -> error_msgf "Couldn't decode port")

let decode_user_pass up =
  match String.split_on_char ':' up with
  | [ _user ] -> Ok None
  | user :: pass ->
      let pass = String.concat ":" pass in
      Ok (Some (user, pass))
  | [] -> assert false

let decode_uri uri =
  let ( >>= ) = Result.bind in
  match String.split_on_char '/' uri with
  | proto :: "" :: user_pass_host_port :: path ->
      begin
        match proto with
        | "pop3:" -> Ok `POP3
        | "git:" -> Ok `Git
        | _ -> error_msgf "Unknown protocol"
      end
      >>= fun protocol ->
      begin
        match String.split_on_char '@' user_pass_host_port with
        | [ host_port ] -> Ok (None, host_port)
        | [ user_pass; host_port ] ->
            decode_user_pass user_pass >>= fun up -> Ok (up, host_port)
        | _ -> error_msgf "Couldn't decode URI"
      end
      >>= fun (user_pass, host_port) ->
      decode_host_port host_port >>= fun (host, port) ->
      let path = "/" ^ String.concat "/" path in
      Ok (Uri (protocol, user_pass, host, port, path))
  | [ user_pass_host_port ] ->
      begin
        match String.split_on_char '@' user_pass_host_port with
        | [ host_port ] -> Ok (None, host_port)
        | [ user_pass; host_port ] ->
            decode_user_pass user_pass >>= fun up -> Ok (up, host_port)
        | _ -> error_msgf "Couldn't decode URI"
      end
      >>= fun (user_pass, host_port) ->
      decode_host_port host_port >>= fun (host, port) ->
      Ok (Uri (`POP3, user_pass, host, port, "/"))
  | _ -> Error (`Msg "Could't decode URI on top")

let decode_ssh str =
  let ( >>= ) = Result.bind in
  let len = String.length str in
  Emile.of_string_raw ~off:0 ~len str |> reword_error (msgf "%a" Emile.pp_error)
  >>= fun (consumed, m) ->
  let rem = String.sub str consumed (len - consumed) in
  match String.split_on_char ':' rem with
  | "" :: path ->
      let local =
        let fn = function `Atom x -> x | `String x -> Fmt.str "%S" x in
        List.map fn m.Emile.local in
      let user = String.concat "." local in
      let host =
        match fst m.Emile.domain with
        | `Domain vs -> String.concat "." vs
        | `Literal v -> v
        | `Addr (Emile.IPv4 v) -> Ipaddr.V4.to_string v
        | `Addr (Emile.IPv6 v) -> Ipaddr.V6.to_string v
        | `Addr (Emile.Ext (k, v)) -> Fmt.str "%s:%s" k v in
      Ok (Git (user, host, String.concat ":" path))
  | _ -> error_msgf "Invalid SSH endpoint"

let remote_of_string str =
  match (decode_ssh str, decode_uri str) with
  | Ok v, _ | _, Ok v -> Ok v
  | _, (Error _ as err) -> err

type cfg = {
  quiet : bool;
  uri : remote;
  authenticator : X509.Authenticator.t option;
  happy_eyeballs : Happy_eyeballs_miou_unix.t;
  excludes : uid list;
  fmt : (string -> string, Format.formatter, unit, string) format4;
}

let or_failwith on_err = function
  | Ok value -> value
  | Error err ->
      let str = on_err err in
      Logs.err (fun m -> m "Task failed with: %s" str) ;
      failwith str

let split_on_char chr ?(interleave = Bstr.make 1 chr) bstr =
  let rec go bottom idx () =
    if idx >= Bstr.length bstr
    then
      let pre = Bstr.sub bstr ~off:bottom ~len:(idx - bottom) in
      Seq.Cons (pre, Seq.empty)
    else if Bstr.get bstr idx = chr
    then
      let pre = Bstr.sub bstr ~off:bottom ~len:(idx - bottom) in
      let seq0 = go (idx + 1) (idx + 1) in
      let seq1 = Seq.cons interleave seq0 in
      Seq.Cons (pre, seq1)
    else go bottom (idx + 1) () in
  go 0 0

let crlf = Bstr.of_string "\r\n"

let run cfg =
  let store stream () =
    let open Flux in
    let save =
      let init = () and merge () () = () in
      Sink.each ~parallel:false ~init ~merge @@ fun (uid, stream) ->
      let filename = Fmt.str cfg.fmt uid in
      Stream.file ~filename stream in
    let into =
      let open Sink.Syntax in
      let+ mails = Sink.list and+ _ = save in
      let fn (uid, _) = Fmt.str cfg.fmt uid in
      List.map fn mails in
    let mails = Stream.into into stream in
    if not cfg.quiet then List.iter print_endline mails in
  match cfg.uri with
  | Git _ | Uri (`Git, _, _, _, _) ->
      let bqueue = Flux.Bqueue.(create with_close) 0x7ff in
      let remote =
        match cfg.uri with
        | Git (user, server, path) -> `SSH (user, server, None, path)
        | Uri (_, _, server, port, path) -> `Git (server, port, path) in
      let fetch =
        Miou.async @@ fun () ->
        Git_miou_unix.fetch remote cfg.happy_eyeballs bqueue
        |> or_failwith (Fmt.str "%a" Git_miou_unix.pp_error) in
      let from = Flux.Source.bqueue bqueue in
      let from = Flux.Stream.from from in
      let from =
        let fn (uid, raw) =
          let uid = Fmt.str "%a" Carton.Uid.pp uid in
          Logs.debug (fun m -> m "[+] %s" uid) ;
          Logs.debug (fun m ->
              m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) (Bstr.to_string raw)) ;
          let seq = split_on_char '\n' ~interleave:crlf raw in
          let src = Flux.Source.seq seq in
          let stream = Flux.Stream.from src in
          let stream = Flux.Stream.map Bstr.to_string stream in
          (uid, stream) in
        Flux.Stream.map fn from in
      let store = Miou.call (store from) in
      Miou.await_exn fetch ;
      Logs.debug (fun m -> m "public-inbox cloned, start to store emails") ;
      Miou.await_exn store
  | Uri (`POP3, authentication, server, port, _) ->
      let ports = Option.map (fun x -> [ x ]) port in
      let stream = Flux.Bqueue.(create with_close) 0x7ff in
      let excludes =
        List.filter_map (function `POP3 uid -> Some uid) cfg.excludes in
      let filter uids =
        let exists uid = List.exists (Pop3.Uid.equal_to_string uid) excludes in
        List.filter (Fun.negate exists) uids in
      let fetch =
        Miou.async @@ fun () ->
        Pop3_miou_unix.fetch ?authentication ?authenticator:cfg.authenticator
          ?ports ~server ~filter cfg.happy_eyeballs stream
        |> or_failwith (Fmt.str "%a" Pop3_miou_unix.pp_error) in
      let from = Flux.Source.bqueue stream in
      let from = Flux.Stream.from from in
      let from =
        let fn (uid, src) =
          let src = Flux.Source.bqueue src in
          let stream = Flux.Stream.from src in
          (Pop3.Uid.to_string uid, stream) in
        Flux.Stream.map fn from in
      let store = Miou.call (store from) in
      Miou.await_exn fetch ;
      Miou.await_exn store

let now () = Some (Ptime_clock.now ())

let run quiet authenticator resolver (uri, _) excludes fmt =
  Miou_unix.run ~domains:2 @@ fun () ->
  let daemon, happy_eyeballs = resolver () in
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let authenticator = Option.map (fun (fn, _) -> fn now) authenticator in
  let finally () =
    Happy_eyeballs_miou_unix.kill daemon ;
    Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  run { quiet; authenticator; happy_eyeballs; uri; excludes; fmt }

open Cmdliner
open Args

let username =
  let doc = "The username used to be connected to the service." in
  let open Arg in
  value
  & opt (some string) None
  & info [ "u"; "username" ] ~doc ~docv:"USERNAME"

let password =
  let doc = "The password used to be connected to the service." in
  let open Arg in
  value
  & opt (some string) None
  & info [ "p"; "password" ] ~doc ~docv:"PASSWORD"

let uri =
  let doc = "The server to fetch emails." in
  let parser str =
    match remote_of_string str with
    | Ok v -> Ok (v, str)
    | Error _ as err -> err in
  let pp ppf (_, str) = Fmt.string ppf str in
  let uri = Arg.conv (parser, pp) in
  let open Arg in
  required & pos 0 (some uri) None & info [] ~doc ~docv:"URI"

let setup_uri (uri : remote * _) username password =
  let str = snd uri in
  match (fst uri, username, password) with
  | _, None, None -> uri
  | Uri (protocol, None, host, port, path), Some username, Some password ->
      let uri = Uri (protocol, Some (username, password), host, port, path) in
      (uri, str)
  | _ -> uri (* TODO(dinosaure): warn which username/password we will use. *)

let setup_uri =
  let open Term in
  const setup_uri $ uri $ username $ password

let authenticator =
  let doc = "The TLS authenticator used to verify TLS certificates." in
  let parser str =
    match X509.Authenticator.of_string str with
    | Ok authenticator -> Ok (authenticator, str)
    | Error _ as err -> err in
  let pp ppf (_, str) = Fmt.string ppf str in
  let authenticator = Arg.conv (parser, pp) in
  let open Arg in
  value
  & opt (some authenticator) None
  & info [ "a"; "auth"; "authenticator" ] ~doc ~docv:"AUTHENTICATOR"

let is_graphic = function '\x21' .. '\x7e' -> true | _ -> false

let uid =
  let parser str =
    match String.split_on_char ':' str with
    | [] -> assert false
    | [ "pop3"; uid ] ->
        (* XXX(dinosaure): See RFC1939. *)
        if String.for_all is_graphic uid
        then Ok (`POP3 uid)
        else error_msgf "Invalid unique email identifier: %S" uid
    | _ -> error_msgf "Invalid unique email identifier: %S" str in
  let pp ppf = function `POP3 uid -> Fmt.pf ppf "pop3:%s" uid in
  Arg.conv (parser, pp)

let excludes =
  let doc = "Excludes some emails to fetch via their unique identifiers." in
  let open Arg in
  value & opt_all uid [] & info [ "exclude" ] ~doc ~docv:"UID"

let default_fmt : (string -> string, Format.formatter, unit, string) format4 =
  "blaze-%s.eml"

let fmt : (string -> string, Format.formatter, unit, string) format4 Term.t =
  let doc = "The format of incoming emails saved into the given directory." in
  let parser str =
    let proof = CamlinternalFormatBasics.(String_ty End_of_fmtty) in
    try Ok (CamlinternalFormat.format_of_string_fmtty str proof)
    with _ -> error_msgf "Invalid format: %S" str in
  let pp ppf (CamlinternalFormatBasics.Format (_, str)) = Fmt.pf ppf "%S" str in
  let fmt = Arg.conv (parser, pp) in
  let open Arg in
  value & opt fmt default_fmt & info [ "f"; "format" ] ~doc ~docv:"FMT"

let term =
  let open Term in
  const run
  $ setup_logs
  $ authenticator
  $ setup_resolver
  $ setup_uri
  $ excludes
  $ fmt

let cmd =
  let doc = "A program to fetch a bunch of emails from a service (like POP3)." in
  let man = [] in
  let info = Cmd.info "fetch" ~doc ~man in
  Cmd.v info term
