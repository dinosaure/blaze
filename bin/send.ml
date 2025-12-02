module Caml_scheduler = Colombe.Sigs.Make (struct
  type +'a t = 'a
end)

let caml =
  let open Caml_scheduler in
  { Colombe.Sigs.bind = (fun x f -> f (prj x)); return = inj }

let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( <.> ) f g x = f (g x)

let to_msg_error =
  Result.map_error @@ function
  | `Msg _ as msg -> msg
  | #Sendmail_with_starttls.error as err ->
      msgf "%a" Sendmail_with_starttls.pp_error err

module Sendmail_unix = struct
  open Colombe

  module Miou_scheduler = Sigs.Make (struct
    type 'a t = 'a
  end)

  let miou =
    let open Miou_scheduler in
    { Sigs.bind = (fun x f -> (f <.> prj) x); return = inj }

  type error = [ `Msg of string | Sendmail_with_starttls.error ]

  let open_sendmail_error = function
    | Ok _ as v -> v
    | Error (#Sendmail.error as err) -> Error err

  let open_sendmail_with_starttls_error = function
    | Ok _ as v -> v
    | Error (#Sendmail_with_starttls.error as err) -> Error err

  let open_error = function
    | Ok _ as v -> v
    | Error (#error as err) -> Error err

  let tcp =
    let open Miou_scheduler in
    let rd flow buf off len =
      match Miou_unix.read flow buf ~off ~len with
      | 0 -> inj `End
      | len -> inj (`Len len)
    and wr flow buf off len = inj (Miou_unix.write flow buf ~off ~len) in
    { Colombe.Sigs.rd; wr }

  let authenticator :
      (X509.Authenticator.t, [ `Msg of string ]) result Miou.Lazy.t =
    Miou.Lazy.from_fun Ca_certs.authenticator

  let tls_config user's_tls_config user's_authenticator =
    match user's_tls_config with
    | Some cfg -> Ok cfg
    | None ->
        let ( let* ) = Result.bind in
        let* authenticator =
          match (Miou.Lazy.force authenticator, user's_authenticator) with
          | Ok authenticator, None -> Ok authenticator
          | _, Some authenticator -> Ok authenticator
          | Error (`Msg msg), None -> Error (`Msg msg) in
        Tls.Config.client ~authenticator ()

  let sendmail ?encoder ?decoder ?queue unix ~domain ?cfg:user's_tls_config
      ?authenticator:user's_authenticator ?authentication sender recipients mail
      =
    let ( let* ) = Result.bind in
    let mail () = Miou_scheduler.inj (mail ()) in
    let* tls_cfg = tls_config user's_tls_config user's_authenticator in
    let socket = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect socket (Unix.ADDR_UNIX (Fpath.to_string unix)) ;
    let socket = Miou_unix.of_file_descr ~non_blocking:true socket in
    let finally () = Miou_unix.close socket in
    Fun.protect ~finally @@ fun () ->
    let ctx =
      Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder ?queue ()
    in
    Sendmail_with_starttls.sendmail miou tcp socket ctx tls_cfg ?authentication
      ~domain sender recipients mail
    |> Miou_scheduler.prj
    |> open_sendmail_with_starttls_error
end

let send authenticator he peer domain sender recipients mail =
  match peer with
  | `Host (domain_name, port) ->
      let destination = Domain_name.to_string domain_name in
      Sendmail_miou_unix.sendmail he ~destination ?port ~domain ?authenticator
        sender recipients mail
      |> to_msg_error
  | `Inet_addr (ipaddr, port) ->
      let destination = Ipaddr.to_string ipaddr in
      Sendmail_miou_unix.sendmail he ~destination ~port ~domain ?authenticator
        sender recipients mail
      |> to_msg_error
  | `Unix path ->
      Sendmail_unix.sendmail path ~domain ?authenticator sender recipients mail
      |> to_msg_error

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

let dry_run domain sender recipients mail =
  let ctx = Colombe.State.Context.make () in
  let rdwr =
    make_rdwr_from_strings
      ([
         "220 blaze";
         "250-Blaze at your service!";
         "250 AUTH LOGIN PLAIN";
         "250 Sender accepted!";
       ]
      @ List.map (fun _ -> "250 Recipient accepted!") recipients
      @ [ "354 "; "250 Sended!"; "221 Closing connection." ]) in
  Sendmail.sendmail caml rdwr () ctx ~domain sender recipients mail
  |> Caml_scheduler.prj
  |> Result.map_error @@ fun err -> msgf "%a" Sendmail.pp_error err

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

let has_at_least_one_domain =
  let open Colombe.Forward_path in
  List.exists @@ function
  | Postmaster -> false
  | Domain _ | Forward_path _ -> true

let first_domain lst =
  let open Colombe.Forward_path in
  List.find
    (function Postmaster -> false | Domain _ | Forward_path _ -> true)
    lst
  |> function
  | Domain domain -> domain
  | Forward_path { Colombe.Path.domain; _ } -> domain
  | _ -> assert false

type peer_name =
  [ `Inet_addr of Ipaddr.t * int
  | `Host of [ `host ] Domain_name.t * int option
  | `Unix of Fpath.t ]

type destination = [ `Dry_run | `Send of peer_name ]

type cfg = {
  authenticator : X509.Authenticator.t option;
  happy_eyeballs : Happy_eyeballs_miou_unix.t;
  destination : destination option;
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
    domain;
    sender;
    recipients;
    mail;
  } =
    cfg in
  let sender = Some sender in
  let mail =
    match mail with
    | None -> stream_of_stdin
    | Some fpath -> stream_of_fpath fpath in
  match (destination, recipients) with
  | Some `Dry_run, _ ->
      let mail () = Caml_scheduler.inj (mail ()) in
      dry_run domain sender recipients mail
  | Some (`Send destination), _ ->
      send authenticator he destination domain sender recipients mail
  | None, _ :: _ when has_at_least_one_domain recipients ->
      let ( let* ) = Result.bind in
      let* destination =
        match first_domain recipients with
        | Colombe.Domain.IPv4 ipv4 -> Ok (`Inet_addr (Ipaddr.V4 ipv4, 25))
        | Colombe.Domain.IPv6 ipv6 -> Ok (`Inet_addr (Ipaddr.V6 ipv6, 25))
        | Colombe.Domain.Domain ds ->
            let* domain_name = Domain_name.of_strings ds in
            let* domain_name = Domain_name.host domain_name in
            Ok (`Host (domain_name, None))
        | Colombe.Domain.Extension (k, v) ->
            error_msgf "We don't support extensible domains: [%s:%s]" k v in
      send authenticator he destination domain sender recipients mail
  | None, _ -> assert false

let now () = Some (Ptime_clock.now ())

let to_exit_status = function
  | Ok () -> `Ok ()
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

let run _ authenticator resolver destination domain sender recipients mail =
  Miou_unix.run ~domains:0 @@ fun () ->
  let daemon, happy_eyeballs = resolver () in
  let authenticator = Option.map (fun (fn, _) -> fn now) authenticator in
  let finally () = Happy_eyeballs_miou_unix.kill daemon in
  Fun.protect ~finally @@ fun () ->
  let cfg =
    {
      authenticator;
      happy_eyeballs;
      destination;
      domain;
      sender;
      recipients;
      mail;
    } in
  run cfg |> to_exit_status

open Cmdliner
open Blaze_cli

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
    let ( >>= ) = Result.bind in
    match Emile.of_string str >>= Colombe_emile.to_path with
    | Ok v -> Ok v
    | Error _ -> error_msgf "Invalid sender: %S" str in
  let pp = Colombe.Path.pp in
  Arg.conv (parser, pp)

let sender =
  let doc = "The sender of the given email." in
  let open Arg in
  required & opt (some sender) None & info [ "s"; "sender" ] ~docv:"SENDER" ~doc

let ( <$> ) f g x = match g x with Ok x -> f x | Error _ as err -> err

let mx =
  let parser str =
    match str with
    | "-" -> Ok `Dry_run
    | str -> (
        let ( let* ) = Result.bind in
        let as_domain =
          match String.split_on_char ':' str with
          | [ str'; port ] -> (
              match int_of_string_opt port with
              | Some port ->
                  let* v = Domain_name.(host <$> of_string) str' in
                  Ok (v, Some port)
              | None ->
                  let* v = Domain_name.(host <$> of_string) str in
                  Ok (v, None))
          | _ ->
              let* v = Domain_name.(host <$> of_string) str in
              Ok (v, None) in
        let as_ipaddr = Ipaddr.with_port_of_string ~default:25 str in
        let as_file = Fpath.of_string str in
        match (as_domain, as_ipaddr, as_file) with
        | _, _, Ok v when Sys.file_exists str -> Ok (`Send (`Unix v))
        | Ok v, _, _ -> Ok (`Send (`Host v))
        | _, Ok (v, port), _ -> Ok (`Send (`Inet_addr (v, port)))
        | _ -> error_msgf "Invalid mail exchange server: %S" str) in
  let pp ppf = function
    | `Dry_run -> Fmt.string ppf "-"
    | `Send (`Host (peer, (Some 25 | None))) ->
        Fmt.pf ppf "%a" Domain_name.pp peer
    | `Send (`Host (peer, Some port)) ->
        Fmt.pf ppf "%a:%d" Domain_name.pp peer port
    | `Send (`Inet_addr (v, port)) -> Fmt.pf ppf "%a:%d" Ipaddr.pp v port
    | `Send (`Unix path) -> Fpath.pp ppf path in
  Arg.conv (parser, pp)

let mx =
  let doc = "Domain name of the mail exchange server." in
  Arg.(value & pos 1 (some mx) None & info [] ~docv:"MX" ~doc)

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
  let ( >>= ) = Result.bind in
  let parser str =
    Emile.of_string str
    |> Result.map_error (fun _err -> msgf "Invalid email %S" str)
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
    | Ok v -> error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.option ~none:(Fmt.any "-") Fpath.pp)

let mail =
  let doc = "The email to check." in
  Arg.(value & pos 0 mail None & info [] ~docv:"MAIL" ~doc)

let cmd =
  let doc = "Send an email to a mail exchange server." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) sends the given email to the specified mail exchange server.";
    ] in
  let open Term in
  let info = Cmd.info "send" ~doc ~man in
  let term =
    const run
    $ setup_logs
    $ authenticator
    $ setup_resolver
    $ mx
    $ hostname
    $ sender
    $ recipients
    $ mail in
  Cmd.v info (ret term)
