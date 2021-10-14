open Rresult

module Caml_scheduler = Colombe.Sigs.Make (struct
  type +'a t = 'a
end)

let caml =
  let open Caml_scheduler in
  { Colombe.Sigs.bind = (fun x f -> f (prj x)); return = inj }

let try_587 inet_addr =
  let sockaddr = Unix.ADDR_INET (inet_addr, 587) in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket domain Unix.SOCK_STREAM 0 in
  match Unix.connect socket sockaddr with
  | () -> Some (`STARTTLS socket)
  | exception _exn -> None

let try_465 inet_addr =
  let sockaddr = Unix.ADDR_INET (inet_addr, 465) in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket domain Unix.SOCK_STREAM 0 in
  match Unix.connect socket sockaddr with
  | () -> Some (`TLS socket)
  | exception _exn -> None

let ( <|> ) f g x = match f x with None -> g x | Some _ as v -> v

let rec fully_write socket str off len =
  if len > 0
  then
    let len' = Unix.write socket (Bytes.unsafe_of_string str) off len in
    fully_write socket str (off + len') (len - len')

let rdwr =
  let open Colombe.Sigs in
  let open Caml_scheduler in
  {
    rd =
      (fun flow buf off len ->
        match Unix.read flow buf off len with
        | 0 -> inj `End
        | len -> inj (`Len len));
    wr = (fun flow str off len -> inj (fully_write flow str off len));
  }

module TLS = struct
  type error =
    | Alert of Tls.Packet.alert_type
    | Failure of Tls.Engine.failure
    | Unix_error of Unix.error * string * string
    | Closed

  let pp_error ppf = function
    | Alert alert ->
        Fmt.pf ppf "TLS alert: %s" (Tls.Packet.alert_type_to_string alert)
    | Failure failure ->
        Fmt.pf ppf "TLS failure: %s" (Tls.Engine.string_of_failure failure)
    | Unix_error (err, f, arg) ->
        Fmt.pf ppf "%s(%s): %s" f arg (Unix.error_message err)
    | Closed -> Fmt.pf ppf "Connection closed by peer"

  type t = {
    socket : Unix.file_descr;
    mutable state : [ `Active of Tls.Engine.state | `Eof | `Error of error ];
    mutable linger : Cstruct.t list;
  }

  let fully_write socket ({ Cstruct.len; _ } as cs) =
    try
      fully_write socket (Cstruct.to_string cs) 0 len ;
      Ok ()
    with Unix.Unix_error (err, f, arg) -> Error (Unix_error (err, f, arg))

  let read socket =
    let buf = Bytes.create 0x1000 in
    match Unix.read socket buf 0 (Bytes.length buf) with
    | 0 -> `Eof
    | len -> `Data (Cstruct.of_bytes ~off:0 ~len buf)
    | exception Unix.Unix_error (err, f, arg) ->
        `Error (Unix_error (err, f, arg))

  let check_write flow res =
    (match (flow.state, res) with
    | `Active _, Error err ->
        flow.state <- `Error err ;
        Unix.close flow.socket
    | _ -> ()) ;
    match res with Ok () -> Ok () | Error err -> Error err

  let read_react flow =
    let handle tls buf =
      match Tls.Engine.handle_tls tls buf with
      | Ok (res, `Response resp, `Data data) ->
          flow.state <-
            (match res with
            | `Ok tls -> `Active tls
            | `Eof -> `Eof
            | `Alert alert -> `Error (Alert alert)) ;
          let _ =
            match resp with
            | None -> Ok ()
            | Some buf -> fully_write flow.socket buf |> check_write flow in
          let () = match res with `Ok _ -> () | _ -> Unix.close flow.socket in
          `Ok data
      | Error (fail, `Response resp) ->
          let reason = `Error (Failure fail) in
          flow.state <- reason ;
          fully_write flow.socket resp |> fun _ ->
          Unix.close flow.socket |> fun () -> reason in
    match flow.state with
    | (`Eof | `Error _) as v -> v
    | `Active _ -> (
        read flow.socket |> function
        | (`Eof | `Error _) as v ->
            flow.state <- v ;
            v
        | `Data buf ->
        match flow.state with
        | `Active tls -> handle tls buf
        | (`Eof | `Error _) as v -> v)

  let rec read flow =
    match flow.linger with
    | [] -> (
        match read_react flow with
        | `Ok None -> read flow
        | `Ok (Some buf) -> Ok (`Data buf)
        | `Eof -> Ok `Eof
        | `Error err -> Error err)
    | bufs ->
        flow.linger <- [] ;
        Ok (`Data (Cstruct.concat (List.rev bufs)))

  let writev flow css =
    match flow.state with
    | `Eof -> Error Closed
    | `Error err -> Error err
    | `Active tls ->
    match Tls.Engine.send_application_data tls css with
    | Some (tls, answer) ->
        flow.state <- `Active tls ;
        fully_write flow.socket answer |> check_write flow
    | None -> assert false

  let write flow cs = writev flow [ cs ]

  let rec drain_handshake flow =
    match flow.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) -> Ok flow
    | _ ->
    match read_react flow with
    | `Ok (Some mbuf) ->
        flow.linger <- mbuf :: flow.linger ;
        drain_handshake flow
    | `Ok None -> drain_handshake flow
    | `Error err -> Error err
    | `Eof -> Error Closed

  let init cfg socket =
    let tls, init = Tls.Engine.client cfg in
    let flow = { socket; state = `Active tls; linger = [] } in
    fully_write socket init |> fun _ -> drain_handshake flow
end

let make_rdwr_with_tls, rdwr_with_tls =
  let module TLS = Unixiz.Make (TLS) in
  let rd flow buf off len =
    let cs = Cstruct.create len in
    match TLS.recv flow cs with
    | Ok (`Input len') ->
        Cstruct.blit_to_bytes cs 0 buf off len' ;
        `Len len'
    | Ok `End_of_flow -> `End
    | Error _ -> `End in
  let wr flow buf off len =
    let cs = Cstruct.of_bytes buf ~off ~len in
    let _ = TLS.send flow cs in
    () in
  let open Colombe.Sigs in
  let open Caml_scheduler in
  ( TLS.make,
    {
      rd = (fun flow buf off len -> inj (rd flow buf off len));
      wr =
        (fun flow str off len ->
          inj (wr flow (Bytes.unsafe_of_string str) off len));
    } )

let setup_authenticator insecure trust_anchor key_fingerprint
    certificate_fingerprint =
  let time () = Some (Ptime_clock.now ()) in
  match (insecure, trust_anchor, key_fingerprint, certificate_fingerprint) with
  | true, _, _, _ -> Ok (fun ?ip:_ ~host:_ _ -> Ok None)
  | _, None, None, None -> Ca_certs.authenticator ()
  | _, Some trust_anchor, None, None ->
      Bos.OS.File.read trust_anchor >>= fun data ->
      X509.Certificate.decode_pem_multiple (Cstruct.of_string data)
      >>| fun cas -> X509.Authenticator.chain_of_trust ~time cas
  | _, None, Some (_host, hash, data), None ->
      let fingerprint = Cstruct.of_string data in
      Ok (X509.Authenticator.server_key_fingerprint ~time ~hash ~fingerprint)
  | _, None, None, Some (_host, hash, data) ->
      let fingerprint = Cstruct.of_string data in
      Ok (X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprint)
  | _ -> R.error_msg "Multiple authenticators provided, expected one"

let pp_peer ppf = function
  | `Inet_addr v -> Ipaddr.V4.pp ppf v
  | `Host v -> Domain_name.pp ppf v

let connect authenticator (inet_addr, peer) ?authentication ~domain sender
    recipients mail =
  let peer_name = match peer with `Host v -> Some v | `Inet_addr _ -> None in
  match (try_465 <|> try_587) inet_addr with
  | Some (`STARTTLS socket) ->
      Logs.debug (fun m -> m "%a:587 with STARTTLS." pp_peer peer) ;
      let ctx = Sendmail_with_starttls.Context_with_tls.make () in
      let cfg = Tls.Config.client ~authenticator ?peer_name () in
      Sendmail_with_starttls.sendmail caml rdwr socket ctx cfg ?authentication
        ~domain sender recipients mail
      |> Caml_scheduler.prj
      |> R.reword_error @@ fun err ->
         R.msgf "%a" Sendmail_with_starttls.pp_error err
  | Some (`TLS socket) -> (
      Logs.debug (fun m -> m "%a:465 with TLS." pp_peer peer) ;
      let cfg = Tls.Config.client ~authenticator ?peer_name () in
      match TLS.init cfg socket with
      | Error err ->
          Logs.err (fun m ->
              m "Got an error when we initiate a TLS connection to %a:587: %a."
                pp_peer peer TLS.pp_error err) ;
          R.error_msgf "Got a TLS error: %a" TLS.pp_error err
      | Ok flow ->
          let ctx = Colombe.State.Context.make () in
          let flow = make_rdwr_with_tls flow in
          Sendmail.sendmail caml rdwr_with_tls flow ctx ?authentication ~domain
            sender recipients mail
          |> Caml_scheduler.prj
          |> R.reword_error @@ fun err -> R.msgf "%a" Sendmail.pp_error err)
  | None -> R.error_msgf "Impossible to communicate with %a" pp_peer peer

let connect authenticator dns
    (peer : [ `Host of 'a Domain_name.t | `Inet_addr of Ipaddr.V4.t ])
    authentication domain sender recipients mail =
  match peer with
  | `Inet_addr ipv4 ->
      connect authenticator
        (Ipaddr_unix.to_inet_addr (Ipaddr.V4 ipv4), peer)
        ?authentication ~domain sender recipients mail
  | `Host host ->
  match Dns_client_unix.gethostbyname dns host with
  | Ok ipv4 ->
      connect authenticator
        (Ipaddr_unix.to_inet_addr (Ipaddr.V4 ipv4), peer)
        ?authentication ~domain sender recipients mail
  | Error _ as err -> err

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
    Caml_scheduler.inj (fully_write Unix.stdout str off len) in
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
      then Caml_scheduler.inj (Some (line ^ "\n", 0, String.length line + 1))
      else Caml_scheduler.inj (Some (line ^ "\r\n", 0, String.length line + 2))
  | exception End_of_file -> Caml_scheduler.inj None

let stream_of_fpath fpath =
  let ic = open_in (Fpath.to_string fpath) in
  let closed = ref false in
  fun () ->
    match input_line ic with
    | line ->
        if String.length line > 0 && line.[String.length line - 1] = '\r'
        then Caml_scheduler.inj (Some (line ^ "\n", 0, String.length line + 1))
        else
          Caml_scheduler.inj (Some (line ^ "\r\n", 0, String.length line + 2))
    | exception End_of_file ->
        if not !closed
        then (
          close_in ic ;
          closed := true) ;
        Caml_scheduler.inj None

let to_exit_status = function
  | Ok () -> `Ok 0
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

let run _ authenticator nameservers timeout peer_name authentication domain
    sender recipients mail =
  let dns = Dns_client_unix.create ?nameservers ~timeout () in
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
  match peer_name with
  | `Dry_run ->
      to_exit_status
        ( domain >>= fun domain ->
          dry_run authentication domain sender recipients mail )
  | `Submission peer_name ->
      to_exit_status
        ( authenticator >>= fun authenticator ->
          domain >>= fun domain ->
          connect authenticator dns peer_name authentication domain sender
            recipients mail )

open Cmdliner

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

let timeout =
  let doc = "The DNS timeout allowed (in nano-second)." in
  Arg.(value & opt int64 5_000_000_000L & info [ "t"; "timeout" ] ~doc)

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

let existing_filename =
  let parser str =
    match Fpath.of_string str with
    | Ok _ as v when Sys.file_exists str -> v
    | Ok v -> R.error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let trust_anchor =
  let doc = "Authenticate TLS using the provided trust anchor." in
  Arg.(value & opt (some existing_filename) None & info [ "trust-anchor" ] ~doc)

let hashes =
  [
    ("md5", `MD5);
    ("sha1", `SHA1);
    ("sha224", `SHA224);
    ("sha256", `SHA256);
    ("sha384", `SHA384);
    ("sha512", `SHA512);
  ]

let parse_hash str =
  match List.assoc_opt (String.lowercase_ascii str) hashes with
  | None -> R.error_msgf "Unknown hash algorithm %S" str
  | Some hash -> Ok hash

let pp_hash ppf hash =
  let str, _ = List.find (fun (_, hash') -> hash = hash') hashes in
  Fmt.string ppf str

let fingerprint =
  let parser str =
    match String.split_on_char ':' str with
    | [ host; alg; data ] ->
        Domain_name.of_string host >>= fun domain_name ->
        Domain_name.host domain_name >>= fun hostname ->
        parse_hash alg >>= fun hash ->
        Base64.decode ~pad:false data >>= fun data -> R.ok (hostname, hash, data)
    | _ -> R.error_msgf "Invalid fingerprint format %S" str in
  let pp ppf (host, alg, data) =
    Fmt.pf ppf "%a:%a:%s" Domain_name.pp host pp_hash alg
      (Base64.encode_string data) in
  Arg.conv (parser, pp)

let insecure =
  let doc = "Don't validate the server certificate." in
  Arg.(value & flag & info [ "insecure" ] ~doc)

let key_fingerprint =
  let doc = "Authenticate TLS using public key fingerprint." in
  Arg.(value & opt (some fingerprint) None & info [ "key-fingerprint" ] ~doc)

let cert_fingerprint =
  let doc = "Authenticator TLS using certificate fingerprint." in
  Arg.(value & opt (some fingerprint) None & info [ "cert-fingerprint" ] ~doc)

let setup_authenticator =
  Term.(
    const setup_authenticator
    $ insecure
    $ trust_anchor
    $ key_fingerprint
    $ cert_fingerprint)

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
    & info [ "s"; "sender" ] ~docv:"<sender>" ~doc)

let submission =
  let parser str =
    match str with
    | "-" -> Ok `Dry_run
    | str ->
    match
      (Domain_name.of_string str >>= Domain_name.host, Ipaddr.V4.of_string str)
    with
    | Ok v, _ -> Ok (`Submission (`Host v))
    | _, Ok v -> Ok (`Submission (`Inet_addr v))
    | _ -> R.error_msgf "Invalid submission server: %S" str in
  let pp ppf = function
    | `Dry_run -> Fmt.string ppf "-"
    | `Submission (`Host peer) -> Domain_name.pp ppf peer
    | `Submission (`Inet_addr v) -> Ipaddr.V4.pp ppf v in
  Arg.conv (parser, pp)

let submission =
  let doc = "Domain name of the SMTP submission server." in
  Arg.(
    required & pos 0 (some submission) None & info [] ~docv:"<submission>" ~doc)

let authentication =
  let doc = "Password (if needed) of the sender." in
  Arg.(value & opt (some string) None & info [ "p"; "password" ] ~doc)

let hostname =
  let parser str = Colombe.Domain.of_string str in
  let pp = Colombe.Domain.pp in
  Arg.conv (parser, pp)

let setup_hostname = function
  | Some v -> Ok v
  | None -> Colombe.Domain.of_string (Unix.gethostname ())

let hostname =
  let doc = "Domain name of the machine." in
  Arg.(value & opt (some hostname) None & info [ "h"; "hostname" ] ~doc)

let setup_hostname = Term.(const setup_hostname $ hostname)

let recipient =
  let parser str =
    Emile.of_string str
    |> R.reword_error (fun _err -> R.msgf "Invalid email %S" str)
    >>= Colombe_emile.to_forward_path in
  Arg.conv (parser, Colombe.Forward_path.pp)

let recipients =
  let doc = "Recipients of the email." in
  Arg.(value & pos_right 1 recipient [] & info [] ~docv:"<recipient>" ~doc)

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
  Arg.(value & pos 1 mail None & info [] ~docv:"<mail>" ~doc)

let cmd =
  let doc = "Send an email to a SMTP submission server." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) sends the given email to the specified SMTP submission \
         server.";
    ] in
  ( Term.(
      ret
        (const run
        $ setup_logs
        $ setup_authenticator
        $ nameserver
        $ timeout
        $ submission
        $ authentication
        $ setup_hostname
        $ sender
        $ recipients
        $ mail)),
    Term.info "send" ~doc ~man )

let () = Term.(exit_status @@ eval cmd)
