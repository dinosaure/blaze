open Rresult

module Caml_scheduler = Colombe.Sigs.Make (struct
  type +'a t = 'a
end)

let caml =
  let open Caml_scheduler in
  { Colombe.Sigs.bind = (fun x f -> f (prj x)); return = inj }

let open_sendmail_with_starttls_error = function
  | Ok _ as v -> v
  | Error #Sendmail_with_starttls.error as err -> err

let submit authenticator he
    (peer : [ `Host of 'a Domain_name.t | `Inet_addr of Ipaddr.t ] * int option)
    authentication domain sender recipients mail =
  let destination =
    match fst peer with
    | `Host domain_name -> Domain_name.to_string domain_name
    | `Inet_addr ipaddr -> Ipaddr.to_string ipaddr in
  Sendmail_miou_unix.submit he ~destination ?port:(snd peer) ~domain
    ?authenticator ?authentication sender recipients mail
  |> Result.map_error @@ function
     | `Msg _ as msg -> msg
     | #Sendmail_with_starttls.error as err ->
         R.msgf "%a" Sendmail_with_starttls.pp_error err

let make_rdwr_from_strings lst =
  let lst = ref (List.map (fun str -> str ^ "\r\n") lst) in
  let rd () buf off len =
    match !lst with
    | [] -> Caml_scheduler.inj `End
    | x :: r ->
        let len = min (String.length x) len in
        Bytes.blit_string x 0 buf off len ;
        if len = String.length x
        then lst := r
        else lst := String.sub x len (String.length x - len) :: r ;
        Caml_scheduler.inj (`Len len) in
  let wr () str off len =
    let _len = Unix.write_substring Unix.stdout str off len in
    Caml_scheduler.inj () in
  { Colombe.Sigs.rd; wr }

let dry_run authentication domain sender recipients mail =
  let ctx = Colombe.State.Context.make () in
  let rdwr =
    make_rdwr_from_strings
      (match authentication with
      | None ->
          [
            "220 blaze";
            "250-Blaze at your service!";
            "250 AUTH LOGIN PLAIN";
            "250 Sender accepted!";
          ]
          @ List.map (fun _ -> "250 Recipient accepted!") recipients
          @ [ "354 "; "250 Sended!"; "221 Closing connection." ]
      | Some _ ->
          [
            "220 blaze";
            "250-Blaze at your service!";
            "250 AUTH LOGIN PLAIN";
            "334 ";
            "235 Authenticated!";
            "250 Sender accepted!";
          ]
          @ List.map (fun _ -> "250 Recipient accepted!") recipients
          @ [ "354 "; "250 Sended!"; "221 Closing connection." ]) in
  Sendmail.sendmail caml rdwr () ctx ?authentication ~domain sender recipients
    mail
  |> Caml_scheduler.prj
  |> R.reword_error @@ fun err -> R.msgf "%a" Sendmail.pp_error err

let stream_of_stdin () =
  match input_line stdin with
  | line ->
      if String.length line > 0 && line.[String.length line - 1] = '\r'
      then Some (line ^ "\n", 0, String.length line + 1)
      else Some (line ^ "\r\n", 0, String.length line + 2)
  | exception End_of_file -> None

let stream_of_fpath fpath =
  let ic = open_in (Fpath.to_string fpath) in
  let closed = ref false in
  fun () ->
    match input_line ic with
    | line ->
        if String.length line > 0 && line.[String.length line - 1] = '\r'
        then Some (line ^ "\n", 0, String.length line + 1)
        else Some (line ^ "\r\n", 0, String.length line + 2)
    | exception End_of_file ->
        if not !closed
        then (
          close_in ic ;
          closed := true) ;
        None

type peer_name =
  [ `Inet_addr of Ipaddr.t | `Host of [ `host ] Domain_name.t ] * int option

type destination = [ `Dry_run | `Submission of peer_name ]

type cfg = {
  authenticator : X509.Authenticator.t option;
  happy_eyeballs : Happy_eyeballs_miou_unix.t;
  destination : destination;
  authentication : string option;
  domain : Colombe.Domain.t;
  sender : Colombe.Path.t;
  recipients : Colombe.Forward_path.t list;
  mail : Fpath.t option;
}

let run cfg =
  let {
    authenticator;
    happy_eyeballs = he;
    destination;
    authentication;
    domain;
    sender;
    recipients;
    mail;
  } =
    cfg in
  let authentication =
    match authentication with
    | Some password ->
        let { Colombe.Path.local; _ } = sender in
        let username =
          match local with
          | `Dot_string vs -> String.concat "." vs
          | `String v -> v in
        Some { Sendmail.username; password; mechanism = Sendmail.PLAIN }
    | None -> None in
  let sender = Some sender in
  let mail =
    match mail with
    | None -> stream_of_stdin
    | Some fpath -> stream_of_fpath fpath in
  match destination with
  | `Dry_run ->
      let mail () = Caml_scheduler.inj (mail ()) in
      dry_run authentication domain sender recipients mail
  | `Submission peer_name ->
      submit authenticator he peer_name authentication domain sender recipients
        mail

let now () = Some (Ptime_clock.now ())

let to_exit_status = function
  | Ok () -> `Ok ()
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

let run _ authenticator resolver destination authentication domain sender
    recipients mail =
  Miou_unix.run ~domains:0 @@ fun () ->
  let daemon, happy_eyeballs = resolver () in
  let authenticator = Option.map (fun (fn, _) -> fn now) authenticator in
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () =
    Happy_eyeballs_miou_unix.kill daemon ;
    Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  let cfg =
    {
      authenticator;
      happy_eyeballs;
      destination;
      authentication;
      domain;
      sender;
      recipients;
      mail;
    } in
  run cfg |> to_exit_status

open Cmdliner
open Args

let docs_tls = "TRANSPORT LAYER SECURITY"

let authenticator =
  let parser str =
    match X509.Authenticator.of_string str with
    | Ok authenticator -> Ok (authenticator, str)
    | Error _ as err -> err in
  let pp ppf (_, str) = Fmt.string ppf str in
  Arg.conv ~docv:"AUTHENTICATOR" (parser, pp)

let authenticator =
  let doc = "The TLS authenticator used to verify TLS certificates." in
  let open Arg in
  value
  & opt (some authenticator) None
  & info
      [ "a"; "auth"; "authenticator" ]
      ~doc ~docs:docs_tls ~docv:"AUTHENTICATOR"

let sender =
  let parser str =
    match R.(Emile.of_string str >>= Colombe_emile.to_path) with
    | Ok v -> Ok v
    | Error _ -> R.error_msgf "Invalid sender: %S" str in
  let pp = Colombe.Path.pp in
  Arg.conv (parser, pp)

let sender =
  let doc = "The sender of the given email." in
  Arg.(
    required
    & opt (some sender) None
    & info [ "s"; "sender" ] ~docv:"SENDER" ~doc)

let submission =
  let parser str =
    match str with
    | "-" -> Ok `Dry_run
    | str -> (
        let str, port =
          match String.split_on_char ':' str with
          | [ str'; port ] -> (
              try
                let port = int_of_string port in
                (str', Some port)
              with _ -> (str, None))
          | _ -> (str, None) in
        match
          (Domain_name.of_string str >>= Domain_name.host, Ipaddr.of_string str)
        with
        | Ok v, _ -> Ok (`Submission (`Host v, port))
        | _, Ok v -> Ok (`Submission (`Inet_addr v, port))
        | _ -> R.error_msgf "Invalid submission server: %S" str) in
  let pp ppf = function
    | `Dry_run -> Fmt.string ppf "-"
    | `Submission (`Host peer, Some port) ->
        Fmt.pf ppf "%a:%d" Domain_name.pp peer port
    | `Submission (`Inet_addr v, Some port) ->
        Fmt.pf ppf "%a:%d" Ipaddr.pp v port
    | `Submission (`Host peer, None) -> Fmt.pf ppf "%a" Domain_name.pp peer
    | `Submission (`Inet_addr v, None) -> Fmt.pf ppf "%a" Ipaddr.pp v in
  Arg.conv (parser, pp)

let submission =
  let doc = "Domain name of the SMTP submission server." in
  Arg.(
    required & pos 1 (some submission) None & info [] ~docv:"SUBMISSION" ~doc)

let authentication =
  let doc = "Password (if needed) of the sender." in
  Arg.(value & opt (some string) None & info [ "p"; "password" ] ~doc)

let hostname =
  let parser str = Colombe.Domain.of_string str in
  let pp = Colombe.Domain.pp in
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
  let hostname = Unix.gethostname () in
  match Colombe.Domain.of_string hostname with
  | Ok domain -> domain
  | Error (`Msg _) ->
      let random_hostname = Colombe.Domain.of_string_exn (generate ~len:16) in
      Logs.warn (fun m ->
          m "Invalid default hostname: %S, use %a as the default hostname"
            hostname Colombe.Domain.pp random_hostname) ;
      random_hostname

let hostname =
  let doc = "Domain name of the machine." in
  let open Arg in
  value & opt hostname default_hostname & info [ "h"; "hostname" ] ~doc

let recipient =
  let parser str =
    Emile.of_string str
    |> R.reword_error (fun _err -> R.msgf "Invalid email %S" str)
    >>= Colombe_emile.to_forward_path in
  Arg.conv (parser, Colombe.Forward_path.pp)

let recipients =
  let doc = "Recipients of the email." in
  let open Arg in
  value
  & opt_all recipient []
  & info [ "r"; "recipient" ] ~docv:"RECIPIENT" ~doc

let mail =
  let parser = function
    | "-" -> Ok None
    | str ->
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok (Some v)
    | Ok v -> R.error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.option ~none:(Fmt.any "-") Fpath.pp)

let mail =
  let doc = "The email to check." in
  Arg.(value & pos 0 mail None & info [] ~docv:"MAIL" ~doc)

let cmd =
  let doc = "Send an email to a SMTP submission server." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) $(b,submit) the given email to the specified SMTP submission \
         server under its identity. Indeed, this tool does not want to \
         $(i,send) an email to someone else but it $(i,submits) an email to an \
         authority which then can send the email to the given recipients under \
         its identity.";
      `P
        "For instance, if the user wants to take advantage of some security \
         frameworks such as $(b,S)ender $(b,Policy) $(b,F)ramework, \
         $(b,D)omain$(b,K)eys $(b,I)dentified $(b,M)ail or $(b,D)omain-based \
         $(b,M)essage $(b,A)uthentication $(i,via) a service, he/she should \
         use $(tname). Otherwise, if the user wants to send an email to a \
         recipient directly, he/she should use $(b,blaze.send).";
    ] in
  let open Term in
  let info = Cmd.info "submit" ~doc ~man in
  let term =
    const run
    $ setup_logs
    $ authenticator
    $ setup_resolver
    $ submission
    $ authentication
    $ hostname
    $ sender
    $ recipients
    $ mail in
  Cmd.v info (ret term)
