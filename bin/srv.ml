open Rresult

let ( % ) f g x = f (g x)
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let add_sub str ~start ~stop acc =
  if start = stop then acc else String.sub str start (stop - start) :: acc

let head str = if String.length str > 0 then Some str.[0] else None

let tail str =
  let len = String.length str in
  if len > 0 then String.sub str 1 (len - 1) else ""

let cuts ~sep s =
  let sep_len = String.length sep in
  if sep_len = 0
  then invalid_arg "Invalid empty separator"
  else
    let s_len = String.length s in
    let max_sep_idx = sep_len - 1 in
    let max_s_idx = s_len - sep_len in
    let rec check_sep start i k acc =
      if k > max_sep_idx
      then
        let new_start = i + sep_len in
        scan new_start new_start (add_sub s ~start ~stop:i acc)
      else if String.get s (i + k) = String.get sep k
      then check_sep start i (k + 1) acc
      else scan start (i + 1) acc
    and scan start i acc =
      if i > max_s_idx
      then
        if start = 0
        then if s_len = 0 then [] else [ s ]
        else List.rev (add_sub s ~start ~stop:s_len acc)
      else if String.get s i = String.get sep 0
      then check_sep start i 1 acc
      else scan start (i + 1) acc in
    scan 0 0 []

module Value = struct
  open Colombe

  type helo = Domain.t
  type pp_220 = string list
  type pp_221 = string list
  type pp_250 = string list
  type tp_354 = string list
  type pn_503 = string list
  type code = int * string list

  type 'x recv =
    | Helo : helo recv
    | Data : unit recv
    | Quit : unit recv
    | Payload : string recv
    | Any : Request.t recv

  type 'x send =
    | PP_220 : pp_220 send
    | PP_221 : pp_221 send
    | PP_250 : pp_250 send
    | TP_354 : tp_354 send
    | PN_503 : pn_503 send
    | Payload : string send
    | Code : code send

  type error =
    [ Request.Decoder.error
    | Reply.Encoder.error
    | `Too_many_bad_commands
    | `Too_many_recipients
    | `Invalid_recipients
    | `No_recipients ]

  let pp_error ppf = function
    | #Reply.Encoder.error as err -> Reply.Encoder.pp_error ppf err
    | #Request.Decoder.error as err -> Request.Decoder.pp_error ppf err
    | `Too_many_bad_commands -> Fmt.string ppf "Too many bad commands"
    | `No_recipients -> Fmt.string ppf "No recipients"
    | `Too_many_recipients -> Fmt.string ppf "Too many recipients"
    | `Invalid_recipients -> Fmt.string ppf "Invalid recipients"
end

module type MONAD = sig
  type context
  type error

  val ( let* ) :
    ('a, 'err) Colombe.State.t ->
    ('a -> ('b, 'err) Colombe.State.t) ->
    ('b, 'err) Colombe.State.t

  val ( let+ ) :
    ('a, 'err) Colombe.State.t ->
    (('a, 'err) result -> ('b, 'err) Colombe.State.t) ->
    ('b, 'err) Colombe.State.t

  val ( >>= ) :
    ('a, 'err) Colombe.State.t ->
    ('a -> ('b, 'err) Colombe.State.t) ->
    ('b, 'err) Colombe.State.t

  val send :
    context ->
    'a Value.send ->
    'a ->
    (unit, [> `Protocol of error ]) Colombe.State.t

  val recv :
    context -> 'a Value.recv -> ('a, [> `Protocol of error ]) Colombe.State.t

  val return : 'a -> ('a, 'err) Colombe.State.t
  val fail : 'err -> ('a, 'err) Colombe.State.t
end

let run : Unix.file_descr -> ('a, 'err) Colombe.State.t -> ('a, 'err) result =
 fun flow state ->
  let rec go = function
    | Colombe.State.Read { buffer; off; len; k } -> (
        match Unix.read flow buffer off len with
        | 0 -> (go % k) `End
        | len -> (go % k) (`Len len))
    | Colombe.State.Write { buffer; off; len; k } ->
        let len = Unix.write flow (Bytes.unsafe_of_string buffer) off len in
        (go % k) len
    | Colombe.State.Return v -> Ok v
    | Colombe.State.Error err -> Error err in
  go state

let receive : type ctx error.
    (module MONAD with type context = ctx and type error = error) ->
    sockaddr:Unix.sockaddr ->
    domain:'host Domain_name.t ->
    Unix.file_descr ->
    ctx ->
    ( _,
      [> `Application of [> `No_recipients ] | `Protocol of error ] )
    Colombe.State.t =
 fun (module Monad) ~sockaddr ~domain flow ctx ->
  let pp_sockaddr ppf = function
    | Unix.ADDR_INET (inet_addr, _) ->
        Fmt.string ppf (Unix.string_of_inet_addr inet_addr)
    | Unix.ADDR_UNIX str -> Fmt.string ppf str in
  let open Monad in
  let open Value in
  let* () = send ctx PP_220 [ Domain_name.to_string domain ] in
  let* domain_from = recv ctx Helo in
  let* () =
    send ctx PP_250
      [
        Fmt.str "%a at your service, [%a]" Domain_name.pp domain pp_sockaddr
          sockaddr;
      ] in
  let politely_close v =
    let* () = send ctx PP_221 [ "Bye, bobby!" ] in
    return v in
  let rec go () =
    let+ command = recv ctx Any in
    match command with
    | Ok `Quit -> politely_close `Quit
    | Ok (`Mail from) ->
        let* () = send ctx PP_250 [ "Ok, buddy!" ] in
        recipients ~from []
    | Error err -> fail err
    | Ok `Reset -> send ctx PP_250 [ "Yes buddy!" ] >>= go
    | Ok _v -> send ctx PN_503 [ "Command out of sequence." ] >>= go
  and recipients ~from acc =
    let* command = recv ctx Any in
    match command with
    | `Data -> (
        match acc with
        | [] ->
            let* () = send ctx Code (554, [ "No recipients!" ]) in
            Error (`Application `No_recipients)
        | _ :: _ ->
            let* () =
              send ctx Value.TP_354 [ "Ok buddy! Finish it with <crlf>.<crlf>" ]
            in
            mail ~from (List.rev acc) (Buffer.create 0x100))
    | `Recipient v ->
        send ctx PP_250 [ "Ok, bobby!" ] >>= fun () ->
        recipients ~from (v :: acc)
    | `Reset -> send ctx PP_250 [ "Ok, bobby!" ] >>= go
    | `Quit -> politely_close `Quit
    | _ ->
        send ctx PN_503 [ "Command out of sequence." ] >>= fun () ->
        recipients ~from acc
  and mail ~from recipients buf =
    run flow (recv ctx Payload) |> function
    | Error err -> Colombe.State.Error err
    | Ok ".." ->
        Buffer.add_char buf '.' ;
        mail ~from recipients buf
    | Ok "." ->
        let* () = send ctx PP_250 [ "Mail sended, buddy!" ] in
        let* () = recv ctx Quit in
        let res = (domain_from, from, recipients, Buffer.contents buf) in
        politely_close (`Mail res)
    | Ok str ->
        Buffer.add_string buf str ;
        Buffer.add_string buf "\r\n" ;
        mail ~from recipients buf in
  go ()

let handle ~sockaddr ~domain flow =
  let open Colombe in
  let module Value = struct
    include Value

    type decoder = Decoder.decoder
    type encoder = Encoder.encoder

    let encode : type a.
        encoder -> a send -> a -> (unit, [> Encoder.error ]) State.t =
     fun encoder w v ->
      let fiber : a send -> [> Encoder.error ] Encoder.state = function
        | Payload ->
            let k encoder =
              Encoder.write v encoder ;
              Encoder.write "\r\n" encoder ;
              Encoder.flush (fun _ -> Encoder.Done) encoder in
            Encoder.safe k encoder
        | PP_220 -> Reply.Encoder.response (`PP_220 v) encoder
        | PP_221 -> Reply.Encoder.response (`PP_221 v) encoder
        | PP_250 -> Reply.Encoder.response (`PP_250 v) encoder
        | TP_354 -> Reply.Encoder.response (`TP_354 v) encoder
        | PN_503 -> Reply.Encoder.response (`PN_503 v) encoder
        | Code ->
            let code, txts = v in
            Reply.Encoder.response (`Other (code, txts)) encoder in
      let rec go = function
        | Encoder.Done -> State.Return ()
        | Encoder.Write { continue; buffer; off; len } ->
            State.Write { k = go % continue; buffer; off; len }
        | Encoder.Error err -> State.Error err in
      (go % fiber) w

    let decode : type a. decoder -> a recv -> (a, [> Decoder.error ]) State.t =
     fun decoder w ->
      let k : Request.t -> (a, [> Decoder.error ]) State.t =
       fun v ->
        match (w, v) with
        | Helo, `Hello v -> Return v
        | Data, `Data -> Return ()
        | Quit, `Quit -> Return ()
        | Payload, `Payload v -> Return v
        | Payload, `Data_end -> Return "."
        | Any, v -> Return v
        | _, v ->
            let v = Fmt.to_to_string Request.pp v in
            Error (`Invalid_command v) in
      let rec go = function
        | Decoder.Done v -> k v
        | Decoder.Read { buffer; off; len; continue } ->
            State.Read { k = go % continue; buffer; off; len }
        | Decoder.Error { error; _ } -> State.Error error in
      go (Request.Decoder.request ~relax:true decoder)
  end in
  let module Monad = struct
    type context = State.Context.t

    include State.Scheduler (State.Context) (Value)
  end in
  let ctx = State.Context.make () in
  let res = receive (module Monad) ~sockaddr ~domain flow ctx in
  run flow res

let handle_with_starttls ~tls ~sockaddr ~domain flow =
  let open Colombe in
  let module Value = struct
    include Value

    type decoder = Decoder.decoder
    type encoder = Encoder.encoder

    let encode : type a.
        encoder -> a send -> a -> (unit, [> Encoder.error ]) State.t =
     fun encoder w v ->
      let fiber : a send -> [> Encoder.error ] Encoder.state = function
        | Payload ->
            let k encoder =
              Encoder.write v encoder ;
              Encoder.write "\r\n" encoder ;
              Encoder.flush (fun _ -> Encoder.Done) encoder in
            Encoder.safe k encoder
        | PP_220 -> Reply.Encoder.response (`PP_220 v) encoder
        | PP_221 -> Reply.Encoder.response (`PP_221 v) encoder
        | PP_250 -> Reply.Encoder.response (`PP_250 v) encoder
        | TP_354 -> Reply.Encoder.response (`TP_354 v) encoder
        | PN_503 -> Reply.Encoder.response (`PN_503 v) encoder
        | Code ->
            let code, txts = v in
            Reply.Encoder.response (`Other (code, txts)) encoder in
      let rec go = function
        | Encoder.Done -> State.Return ()
        | Encoder.Write { continue; buffer; off; len } ->
            State.Write { k = go % continue; buffer; off; len }
        | Encoder.Error err -> State.Error err in
      (go % fiber) w

    let decode : type a. decoder -> a recv -> (a, [> Decoder.error ]) State.t =
     fun decoder w ->
      let k : Request.t -> (a, [> Decoder.error ]) State.t =
       fun v ->
        match (w, v) with
        | Helo, `Hello v -> Return v
        | Data, `Data -> Return ()
        | Quit, `Quit -> Return ()
        | Payload, `Payload v -> Return v
        | Payload, `Data_end -> Return "."
        | Any, v -> Return v
        | _, v ->
            let v = Fmt.to_to_string Request.pp v in
            Error (`Invalid_command v) in
      let rec go = function
        | Decoder.Done v -> k v
        | Decoder.Read { buffer; off; len; continue } ->
            State.Read { k = go % continue; buffer; off; len }
        | Decoder.Error { error; _ } -> State.Error error in
      go (Request.Decoder.request ~relax:true decoder)

    let encode_without_tls ctx v w =
      let rec go = function
        | State.Read { k; buffer; off; len } ->
            State.Read { k = go % k; buffer; off; len }
        | State.Write { k; buffer; off; len } ->
            State.Write { k = go % k; buffer; off; len }
        | State.Return v -> Return v
        | State.Error err -> Error err in
      go (encode ctx v w)

    let decode_without_tls ctx w =
      let rec go = function
        | State.Read { k; buffer; off; len } ->
            State.Read { k = go % k; buffer; off; len }
        | State.Write { k; buffer; off; len } ->
            State.Write { k = go % k; buffer; off; len }
        | State.Return v -> Return v
        | State.Error err -> Error err in
      go (decode ctx w)
  end in
  let module Value_with_tls = Sendmail_with_starttls.Make_with_tls (Value) in
  let module Monad = struct
    type context = Sendmail_with_starttls.Context_with_tls.t

    include
      State.Scheduler (Sendmail_with_starttls.Context_with_tls) (Value_with_tls)
  end in
  let ctx = Sendmail_with_starttls.Context_with_tls.make () in
  let res =
    let open Monad in
    let* command = recv ctx Any in
    match command with
    | `Verb ("STARTTLS", []) ->
        let* () = send ctx PP_220 [ "Go ahead buddy!" ] in
        let decoder = Sendmail_with_starttls.Context_with_tls.decoder ctx in
        let tls_error err = `Tls err in
        Value_with_tls.starttls_as_server decoder tls |> reword_error tls_error
        >>= fun () -> receive (module Monad) ~sockaddr ~domain flow ctx
    | _ -> assert false in
  run flow res

type tls_error =
  [ `Tls_alert of Tls.Packet.alert_type
  | `Tls_closed
  | `Tls_failure of Tls.Engine.failure ]

type application_error = [ `No_recipients ]

let pp_error ppf = function
  | `Tls (`Tls_alert alert) ->
      Fmt.pf ppf "%s" (Tls.Packet.alert_type_to_string alert)
  | `Tls `Tls_closed -> Fmt.pf ppf "TLS connection closed by peer"
  | `Tls (`Tls_failure failure) ->
      Fmt.pf ppf "%s" (Tls.Engine.string_of_failure failure)
  | `Protocol err -> Fmt.pf ppf "%a" Value.pp_error err
  | `Application `No_recipients -> Fmt.string ppf "No recipients"

let pp_sockaddr ppf = function
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
  | Unix.ADDR_UNIX str -> Fmt.pf ppf "%s" str

let pp_recipient ppf (recipient, _) = Colombe.Forward_path.pp ppf recipient

let show ~with_metadata ~domain_from ~from:(from, _) ~recipients mail output =
  if with_metadata
  then (
    Fmt.pr "domain-from: %a\n%!" Colombe.Domain.pp domain_from ;
    Fmt.pr "from       : %a\n%!" Colombe.Reverse_path.pp from ;
    Fmt.pr "recipients : @[<hov>%a@]\n%!"
      Fmt.(Dump.list pp_recipient)
      recipients) ;
  match output with
  | `Simple path ->
      let oc = open_out (Fpath.to_string path) in
      output_string oc mail
  | `Multiple (dir, fmt) -> (
      let ( let* ) = Result.bind in
      (let* path = Bos.OS.File.tmp ~dir fmt in
       let oc = open_out (Fpath.to_string path) in
       output_string oc mail ;
       close_out oc ;
       Ok ())
      |> function
      | Ok () -> ()
      | Error (`Msg msg) -> Fmt.epr "%s.\n%!" msg)

let is_simple = function `Simple _ -> true | `Multiple _ -> false

let serve with_metadata kind sockaddr domain output =
  let socket =
    Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket SO_REUSEPORT true ;
  let lift = function
    | Ok v -> Ok v
    | Error (`Protocol (`Value err)) -> Error (`Protocol err)
    | Error (`Protocol (#tls_error as tls_error)) -> Error (`Tls tls_error)
    | Error (`Tls (`Value err)) -> Error (`Protocol err)
    | Error (`Tls (#tls_error as tls_error)) -> Error (`Tls tls_error)
    | Error (`Application err) -> Error (`Application err) in
  let rec go socket =
    let flow, peer = Unix.accept socket in
    let res =
      let finally () = Unix.close flow in
      Fun.protect ~finally @@ fun () ->
      match kind with
      | `Clear -> handle ~sockaddr ~domain flow
      | `Tls tls -> handle_with_starttls ~tls ~sockaddr ~domain flow |> lift
    in
    match res with
    | Ok `Quit -> go socket
    | Ok (`Mail (domain_from, from, recipients, mail)) ->
        show ~with_metadata ~domain_from ~from ~recipients mail output ;
        if is_simple output then Ok (Unix.close socket) else go socket
    | Error err ->
        Fmt.epr "[%a][%a]: %a.\n%!"
          Fmt.(styled `Red string)
          "ERROR"
          Fmt.(styled `Cyan pp_sockaddr)
          peer pp_error err ;
        go socket in
  try
    Unix.bind socket sockaddr ;
    Unix.listen socket 1 ;
    go socket
  with Unix.Unix_error (err, f, arg) ->
    error_msgf "%s(%s): %s." f arg (Unix.error_message err)

let load_file path =
  let ic = open_in (Fpath.to_string path) in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ;
  close_in ic ;
  Bytes.unsafe_to_string rs

let private_key_of_file path = X509.Private_key.decode_pem (load_file path)
let certificate_of_file path = X509.Certificate.decode_pem (load_file path)

let sockaddr_of_bind_name = function
  | `Inet_addr (inet_addr, port) ->
      Ok (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr inet_addr, port))
  | `Unix path -> Ok (Unix.ADDR_UNIX (Fpath.to_string path))
  | `Host (host, port) ->
  match Unix.gethostbyname (Domain_name.to_string host) with
  | { Unix.h_addr_list = [||]; _ } ->
      R.error_msgf "Unknown domain-name %a" Domain_name.pp host
  | { Unix.h_addr_list; _ } -> Ok (Unix.ADDR_INET (h_addr_list.(0), port))

type output =
  [ `Simple of Fpath.t | `Multiple of Fpath.t * Bos.OS.File.tmp_name_pat ]

let run with_metadata pk cert bind_name domain (output : output) =
  let ( let* ) = Result.bind in
  let* sockaddr = sockaddr_of_bind_name bind_name in
  match (pk, cert) with
  | None, None -> serve with_metadata `Clear sockaddr domain output
  | Some pk, Some cert -> (
      match (private_key_of_file pk, certificate_of_file cert) with
      | Ok pk, Ok cert ->
          let* tls =
            Tls.Config.server ~certificates:(`Single ([ cert ], pk)) () in
          serve with_metadata (`Tls tls) sockaddr domain output
      | (Error (`Msg _) as err), _ | _, (Error (`Msg _) as err) -> err)
  | Some _, None | None, Some _ ->
      error_msgf "Missing elements to initiate a STARTTLS server."

let run _ with_metadata pk cert bind_name domain output =
  match run with_metadata pk cert bind_name domain output with
  | Ok () -> `Ok ()
  | Error (`Msg msg) -> `Error (false, Fmt.str "%s." msg)

open Cmdliner
open Args

let bind_name =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok (`Unix v)
    | _ ->
    match String.split_on_char ':' str with
    | [ addr; port ] -> (
        match
          ( Ipaddr.of_string addr,
            Domain_name.(of_string addr >>= host),
            int_of_string port )
        with
        | Ok inet_addr, _, port -> Ok (`Inet_addr (inet_addr, port))
        | _, Ok host, port -> Ok (`Host (host, port))
        | _ -> R.error_msgf "Invalid bind address: %S" str
        | exception _ -> R.error_msgf "Invalid bind address: %S" str)
    | [ addr ] -> (
        match
          (Ipaddr.of_string addr, Domain_name.(of_string addr >>= host))
        with
        | Ok inet_addr, _ -> Ok (`Inet_addr (inet_addr, 25))
        | _, Ok host -> Ok (`Host (host, 25))
        | _ -> R.error_msgf "Invalid bind address: %S" str)
    | _ -> R.error_msgf "Invalid bind address: %S" str in
  let pp ppf = function
    | `Inet_addr (inet_addr, 25) -> Fmt.pf ppf "%a" Ipaddr.pp inet_addr
    | `Inet_addr (inet_addr, port) ->
        Fmt.pf ppf "%a:%d" Ipaddr.pp inet_addr port
    | `Host (host, 25) -> Domain_name.pp ppf host
    | `Host (host, port) -> Fmt.pf ppf "%a:%d" Domain_name.pp host port
    | `Unix path -> Fpath.pp ppf path in
  Arg.conv (parser, pp)

let default_bind_name = `Inet_addr (Ipaddr.V4 Ipaddr.V4.any, 25)

let bind_name =
  let doc = "The address where the server listens." in
  let open Arg in
  value & pos 0 bind_name default_bind_name & info [] ~docv:"ADDRESS" ~doc

let fmt :
    (string -> string, Format.formatter, unit, string) format4 option Term.t =
  let doc = "The format of incoming emails saved into the given directory." in
  let parser str =
    let proof = CamlinternalFormatBasics.(String_ty End_of_fmtty) in
    try Ok (CamlinternalFormat.format_of_string_fmtty str proof)
    with _ -> error_msgf "Invalid format: %S" str in
  let pp ppf (CamlinternalFormatBasics.Format (_, str)) = Fmt.pf ppf "%S" str in
  let fmt = Arg.conv (parser, pp) in
  let open Arg in
  value & opt (some fmt) None & info [ "format" ] ~doc

let output =
  let doc = "The path of the received email." in
  let parser str =
    match Fpath.of_string str with
    | Ok v ->
        if Sys.file_exists str && Sys.is_directory str
        then Ok (Fpath.to_dir_path v)
        else if Sys.file_exists str
        then error_msgf "%a already exists" Fpath.pp v
        else Ok v
    | Error _ as err -> err in
  let output = Arg.conv (parser, Fpath.pp) in
  Arg.(value & opt (some output) None & info [ "o"; "output" ] ~doc)

let default_fmt : (string -> string, Format.formatter, unit, string) format4 =
  "blaze-%s.eml"

let setup_output output fmt : (output, [> `Msg of string ]) result =
  let ( let* ) = Result.bind in
  match (output, fmt) with
  | None, Some _ ->
      Result.error (`Msg "A directory is required to store incoming emails.")
  | Some output, fmt ->
      if Fpath.is_dir_path output
      then
        let* _ = Bos.OS.Dir.create ~path:true output in
        let fmt = Option.value ~default:default_fmt fmt in
        Ok (`Multiple (output, fmt))
      else (
        if Option.is_some fmt
        then
          Log.warn (fun m ->
              m
                "The format argument is useless, we will handle only one \
                 incoming email") ;
        Ok (`Simple output))
  | None, None ->
      let* tmp = Bos.OS.File.tmp "blaze-%s.eml" in
      Fmt.pr "The incoming email will be saved into: %a\n%!" Fpath.pp tmp ;
      Ok (`Simple tmp)

let setup_output =
  let open Term in
  term_result ~usage:true (const setup_output $ output $ fmt)

let default_domain = R.get_ok (Domain_name.of_string (Unix.gethostname ()))
let domain = Arg.conv (Domain_name.of_string, Domain_name.pp)

let domain =
  let doc = "Hostname of the machine." in
  Arg.(value & opt domain default_domain & info [ "h"; "hostname" ] ~doc)

let private_key =
  let doc = "Private key to initiate the STARTTLS extension." in
  Arg.(value & opt (some existing_file) None & info [ "k"; "key" ] ~doc)

let certificate =
  let doc = "Certificate to initiate the STARTTLS extension." in
  Arg.(value & opt (some existing_file) None & info [ "c"; "certificate" ] ~doc)

let with_metadata =
  let doc =
    "Show $(i,meta-data) recorded by the server. If this option is not used \
     with $(b,-o), the output will use the NEWLINE convention instead to print \
     out the incoming email instead of CRLF." in
  Arg.(value & flag & info [ "with-data" ] ~doc)

let cmd =
  let doc = "Initiate a SMTP server to receive $(b,one) email." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) launches a SMTP server to receive an email.";
    ] in
  let term =
    let open Term in
    ret
      (const run
      $ setup_logs
      $ with_metadata
      $ private_key
      $ certificate
      $ bind_name
      $ domain
      $ setup_output) in
  Cmd.v (Cmd.info "srv" ~doc ~man) term
