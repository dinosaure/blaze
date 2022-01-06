open Rresult

module Caml_scheduler = Colombe.Sigs.Make (struct
  type +'a t = 'a
end)

let caml =
  let open Caml_scheduler in
  { Colombe.Sigs.bind = (fun x f -> f (prj x)); return = inj }

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

let pp_sockaddr ppf = function
  | Unix.ADDR_INET (inet_addr, 25) ->
      Fmt.string ppf (Unix.string_of_inet_addr inet_addr)
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
  | Unix.ADDR_UNIX path -> Fmt.string ppf path

let connect authenticator (sockaddr, peer_name) ~domain sender recipients mail =
  let socket =
    Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  match Unix.connect socket sockaddr with
  | () -> (
      let ctx = Sendmail_with_starttls.Context_with_tls.make () in
      let cfg = Tls.Config.client ~authenticator ?peer_name () in
      Sendmail_with_starttls.sendmail caml rdwr socket ctx cfg ~domain sender
        recipients mail
      |> Caml_scheduler.prj
      |> fun res ->
      Unix.close socket ;
      match res with
      | Ok v -> Ok v
      | Error `STARTTLS_unavailable -> (
          let socket =
            Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
          in
          match Unix.connect socket sockaddr with
          | () ->
              let ctx = Colombe.State.Context.make () in
              Sendmail.sendmail caml rdwr socket ctx ~domain sender recipients
                mail
              |> Caml_scheduler.prj
              |> fun res ->
              Unix.close socket ;
              res
              |> R.reword_error @@ fun err -> R.msgf "%a" Sendmail.pp_error err
          | exception _ ->
          match peer_name with
          | Some peer_name ->
              R.error_msgf "Impossible to communicate with %a" Domain_name.pp
                peer_name
          | None ->
              R.error_msgf "Impossible to communicate with %a" pp_sockaddr
                sockaddr)
      | Error _ as err ->
          err
          |> R.reword_error @@ fun err ->
             R.msgf "%a" Sendmail_with_starttls.pp_error err)
  | exception _ ->
  match peer_name with
  | Some peer_name ->
      R.error_msgf "Impossible to communicate with %a" Domain_name.pp peer_name
  | None ->
      R.error_msgf "Impossible to communicate with %a" pp_sockaddr sockaddr

let connect authenticator dns peer port domain sender recipients mail =
  match peer with
  | `Inet_addr ip ->
      let port = match port with Some port -> port | None -> 25 in
      connect authenticator
        (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port), None)
        ~domain sender recipients mail
  | `Unix path ->
      connect authenticator
        (Unix.ADDR_UNIX (Fpath.to_string path), None)
        ~domain sender recipients mail
  | `Host peer_name -> (
      let port = match port with Some port -> port | None -> 25 in
      match
        ( Dns_client_unix.gethostbyname6 dns peer_name,
          Dns_client_unix.gethostbyname dns peer_name )
      with
      | Ok ipv6, _ ->
          connect authenticator
            ( Unix.ADDR_INET (Ipaddr_unix.to_inet_addr (Ipaddr.V6 ipv6), port),
              Some peer_name )
            ~domain sender recipients mail
      | _, Ok ipv4 ->
          connect authenticator
            ( Unix.ADDR_INET (Ipaddr_unix.to_inet_addr (Ipaddr.V4 ipv4), port),
              Some peer_name )
            ~domain sender recipients mail
      | (Error _ as err), _ -> err)

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

let has_at_least_one_domain =
  List.exists @@ function
  | Colombe.Forward_path.Postmaster -> false
  | Colombe.Forward_path.Domain _ | Colombe.Forward_path.Forward_path _ -> true

let first_domain lst =
  List.find
    (function
      | Colombe.Forward_path.Postmaster -> false
      | Colombe.Forward_path.Domain _ | Colombe.Forward_path.Forward_path _ ->
          true)
    lst
  |> function
  | Colombe.Forward_path.Domain domain -> domain
  | Colombe.Forward_path.Forward_path { Colombe.Path.domain; _ } -> domain
  | _ -> assert false

let run _ authenticator nameservers timeout peer_name domain sender recipients
    mail =
  let dns = Dns_client_unix.create ?nameservers ~timeout () in
  let mail =
    match mail with
    | None -> stream_of_stdin
    | Some fpath -> stream_of_fpath fpath in
  match (peer_name, recipients) with
  | Some `Dry_run, _ ->
      let sender = Some sender in
      to_exit_status
        (domain >>= fun domain -> dry_run domain sender recipients mail)
  | Some (`Submission (peer_name, port)), _ ->
      let sender = Some sender in
      to_exit_status
        ( authenticator >>= fun authenticator ->
          domain >>= fun domain ->
          connect authenticator dns peer_name port domain sender recipients mail
        )
  | None, _ :: _ when has_at_least_one_domain recipients -> (
      match first_domain recipients with
      | Colombe.Domain.IPv4 ipv4 ->
          let sender = Some sender in
          to_exit_status
            ( authenticator >>= fun authenticator ->
              domain >>= fun domain ->
              connect authenticator dns (`Inet_addr (Ipaddr.V4 ipv4)) None
                domain sender recipients mail )
      | Colombe.Domain.IPv6 ipv6 ->
          let sender = Some sender in
          to_exit_status
            ( authenticator >>= fun authenticator ->
              domain >>= fun domain ->
              connect authenticator dns (`Inet_addr (Ipaddr.V6 ipv6)) None
                domain sender recipients mail )
      | Colombe.Domain.Domain lst ->
          let sender = Some sender in
          to_exit_status
            ( authenticator >>= fun authenticator ->
              domain >>= fun domain ->
              connect authenticator dns
                (`Host Domain_name.(host_exn (of_strings_exn lst)))
                None domain sender recipients mail )
      | Colombe.Domain.Extension (k, v) ->
          `Error
            (false, Fmt.str "We don't support extensible domains: %s:%s" k v))
  | _ ->
      `Error
        (true, "Missing a destination (as a recipient or as the SMTP server)")

open Cmdliner
open Args

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
          ( Domain_name.of_string str >>= Domain_name.host,
            Ipaddr.of_string str,
            Fpath.of_string str )
        with
        | _, _, Ok v when Sys.file_exists str ->
            Ok (`Submission (`Unix v, port))
        | Ok v, _, _ -> Ok (`Submission (`Host v, port))
        | _, Ok v, _ -> Ok (`Submission (`Inet_addr v, port))
        | _ -> R.error_msgf "Invalid submission server: %S" str) in
  let pp ppf = function
    | `Dry_run -> Fmt.string ppf "-"
    | `Submission (`Host peer, (Some 25 | None)) ->
        Fmt.pf ppf "%a" Domain_name.pp peer
    | `Submission (`Inet_addr v, (Some 25 | None)) ->
        Fmt.pf ppf "%a" Ipaddr.pp v
    | `Submission (`Host peer, Some port) ->
        Fmt.pf ppf "%a:%d" Domain_name.pp peer port
    | `Submission (`Inet_addr v, Some port) ->
        Fmt.pf ppf "%a:%d" Ipaddr.pp v port
    | `Submission (`Unix path, _) -> Fpath.pp ppf path in
  Arg.conv (parser, pp)

let submission =
  let doc = "Domain name of the SMTP submission server." in
  Arg.(value & pos 1 (some submission) None & info [] ~docv:"<submission>" ~doc)

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
  Arg.(
    value
    & opt_all recipient []
    & info [ "r"; "recipient" ] ~docv:"<recipient>" ~doc)

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
  Arg.(value & pos 0 mail None & info [] ~docv:"<mail>" ~doc)

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
        $ setup_hostname
        $ sender
        $ recipients
        $ mail)),
    Term.info "send" ~doc ~man )

let () = Term.(exit_status @@ eval cmd)
