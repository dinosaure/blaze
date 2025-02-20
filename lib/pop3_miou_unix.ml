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

module Stream = struct
  type 'a t = {
    arr : 'a option array;
    lock : Miou.Mutex.t;
    mutable read_pos : int;
    mutable write_pos : int;
    mutable closed : bool;
    not_empty_or_closed : Miou.Condition.t;
    not_full : Miou.Condition.t;
  }

  let make len =
    {
      arr = Array.make len None;
      lock = Miou.Mutex.create ();
      read_pos = 0;
      write_pos = 0;
      closed = false;
      not_empty_or_closed = Miou.Condition.create ();
      not_full = Miou.Condition.create ();
    }

  let put t data =
    Miou.Mutex.protect t.lock @@ fun () ->
    match (data, t.closed) with
    | None, true -> ()
    | None, false ->
        t.closed <- true ;
        Miou.Condition.signal t.not_empty_or_closed
    | Some _, true -> invalid_arg "Stream.put: stream already closed"
    | (Some _ as data), false ->
        while (t.write_pos + 1) mod Array.length t.arr = t.read_pos do
          Miou.Condition.wait t.not_full t.lock
        done ;
        t.arr.(t.write_pos) <- data ;
        t.write_pos <- (t.write_pos + 1) mod Array.length t.arr ;
        Miou.Condition.signal t.not_empty_or_closed

  let get t =
    Miou.Mutex.protect t.lock @@ fun () ->
    while t.write_pos = t.read_pos && not t.closed do
      Miou.Condition.wait t.not_empty_or_closed t.lock
    done ;
    if t.write_pos = t.read_pos && t.closed
    then None
    else begin
      let data = Option.get t.arr.(t.read_pos) in
      t.arr.(t.read_pos) <- None ;
      t.read_pos <- (t.read_pos + 1) mod Array.length t.arr ;
      Miou.Condition.signal t.not_full ;
      Some data
    end

  let to_seq t =
    let dispenser () = get t in
    Seq.of_dispenser dispenser
end

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
    let mail = Stream.make 0x100 in
    let push = Stream.put mail in
    Stream.put stream (Some (uid, mail)) ;
    push in
  let finally () = Stream.put stream None in
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
