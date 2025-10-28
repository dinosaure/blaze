let src = Logs.Src.create "pop3.miou.unix"

module Log = (val Logs.src_log src : Logs.LOG)

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

let rec clear fd = function
  | Protocol.Error err -> Result.Error err
  | Return v -> Ok v
  | Read { k; buffer; off; len } ->
      let len = Miou_unix.read fd ~off ~len buffer in
      let res = if len = 0 then `End else `Len len in
      clear fd (k res)
  | Write { k; buffer; off; len } ->
      Miou_unix.write fd ~off ~len buffer ;
      clear fd (k len)

let rec tls fd = function
  | Protocol.Error err -> Result.Error err
  | Return v -> Ok v
  | Read { k; buffer; off; len } ->
      let len = Tls_miou_unix.read fd ~off ~len buffer in
      let res = if len = 0 then `End else `Len len in
      tls fd (k res)
  | Write { k; buffer; off; len } ->
      Tls_miou_unix.write fd buffer ~off ~len ;
      tls fd (k len)

let ( $ ) f g x = match f x with Ok x -> g x | Error _ as err -> err

type error = [ Pop3.error | `Msg of string ]

let pp_error ppf = function
  | #Pop3.error as err -> Pop3.pp_error ppf err
  | `Msg msg -> Fmt.string ppf msg

let fetch ?authentication ?cfg:user's_tls_config
    ?authenticator:user's_authenticator ?(ports = [ 110; 995 ]) ~server
    ~filter:choose he stream =
  let ( let* ) = Result.bind in
  let* tls_cfg = tls_config user's_tls_config user's_authenticator in
  let* (_, port), socket = Happy_eyeballs_miou_unix.connect he server ports in
  Log.debug (fun m -> m "Connected to %s:%d" server port) ;
  let finally () = Miou_unix.close socket in
  Fun.protect ~finally @@ fun () ->
  let protocol =
    match (port, authentication) with
    | 110, None -> `Clear
    | 110, Some _ ->
        Log.err (fun m ->
            m "Unallowed to start a clear connection with an authentication") ;
        Fmt.failwith
          "We don't initiate a clear POP3 connection with an authentication"
    | _ -> `Tls tls_cfg in
  let ctx = Protocol.ctx () in
  let emitter_of ~uid =
    let mail = Flux.Bqueue.(create with_close) 0x7ff in
    let push = function
      | Some chunk -> Flux.Bqueue.put mail chunk
      | None -> Flux.Bqueue.close mail in
    Flux.Bqueue.put stream (uid, mail) ;
    push in
  let finally () = Flux.Bqueue.close stream in
  Fun.protect ~finally @@ fun () ->
  Log.debug (fun m -> m "fetch emails from %s" server) ;
  let t = Pop3.fetch ?authentication ~choose ~emitter_of ctx in
  match protocol with
  | `Clear -> clear socket t
  | `Tls cfg ->
      Log.debug (fun m -> m "Start a TLS connection with %s:%d" server port) ;
      let host =
        match Domain_name.(of_string $ host) server with
        | Ok host -> Some host
        | Error _ -> None in
      let fd = Tls_miou_unix.client_of_fd cfg ?host socket in
      tls fd t
