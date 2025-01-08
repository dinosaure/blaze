let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

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
  | [ proto; ""; user_pass_host_port ] ->
      begin
        match proto with
        | "pop3:" -> Ok `POP3
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
      Ok (protocol, user_pass, host, port)
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
      Ok (`POP3, user_pass, host, port)
  | _ -> error_msgf "Could't decode URI on top"

type uid = [ `POP3 of string ]
type protocol = [ `POP3 ]
type uri = protocol * (string * string) option * string * int option

type cfg = {
  quiet : bool;
  uri : uri;
  authenticator : X509.Authenticator.t option;
  happy_eyeballs : Happy_eyeballs_miou_unix.t;
  excludes : uid list;
  fmt : (string -> string, Format.formatter, unit, string) format4;
}

let run cfg =
  let stream = Pop3_miou_unix.Stream.make 0x100 in
  let protocol, authentication, server, port = cfg.uri in
  let ports = match port with Some port -> Some [ port ] | None -> None in
  let store =
    Miou.call @@ fun () ->
    let rec go mails =
      match Pop3_miou_unix.Stream.get stream with
      | None -> if not cfg.quiet then List.iter print_endline mails
      | Some (uid, mail) ->
          Logs.debug (fun m -> m "Store %a" Pop3.Uid.pp uid) ;
          let filename = Fmt.str cfg.fmt (Pop3.Uid.to_string uid) in
          let oc = open_out filename in
          let finally () = close_out oc in
          begin
            Fun.protect ~finally @@ fun () ->
            let seq = Pop3_miou_unix.Stream.to_seq mail in
            Seq.iter (output_string oc) seq
          end ;
          go (filename :: mails) in
    go [] in
  match protocol with
  | `POP3 ->
      let excludes =
        List.filter_map (function `POP3 uid -> Some uid) cfg.excludes in
      let filter uids =
        let exists uid = List.exists (Pop3.Uid.equal_to_string uid) excludes in
        List.filter (Fun.negate exists) uids in
      let fetch =
        Miou.async @@ fun () ->
        match
          Pop3_miou_unix.fetch ?authentication ?authenticator:cfg.authenticator
            ?ports ~server ~filter cfg.happy_eyeballs stream
        with
        | Ok () -> ()
        | Error err -> Fmt.failwith "%a" Pop3_miou_unix.pp_error err in
      Miou.await_exn fetch ;
      Miou.await_exn store

let now () = Some (Ptime_clock.now ())

let run quiet authenticator (daemon, happy_eyeballs) (uri, _) excludes fmt =
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
    match decode_uri str with Ok v -> Ok (v, str) | Error _ as err -> err in
  let pp ppf (_, str) = Fmt.string ppf str in
  let uri = Arg.conv (parser, pp) in
  let open Arg in
  required & pos 0 (some uri) None & info [] ~doc ~docv:"URI"

let setup_uri (uri : uri * _) username password =
  let str = snd uri in
  match (fst uri, username, password) with
  | _, None, None -> uri
  | (protocol, None, host, port), Some username, Some password ->
      let uri = (protocol, Some (username, password), host, port) in
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

let () = Miou_unix.run @@ fun () -> Cmd.(exit @@ eval cmd)
