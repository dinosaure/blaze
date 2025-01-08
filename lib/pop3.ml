let src = Logs.Src.create "pop3"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Protocol.bind

let transmit ~emitter ctx =
  let rec go () =
    let* str = Protocol.decode ctx in
    match str with
    | "." ->
        emitter None ;
        Protocol.return ()
    | ".." ->
        emitter (Some ".\r\n") ;
        go ()
    | str ->
        emitter (Some (str ^ "\r\n")) ;
        go () in
  go ()

module Uid = struct
  type t = { sid : int; uid : string }

  let equal_to_string { uid; _ } str = String.equal uid str
  let pp ppf { uid; _ } = Fmt.string ppf uid
  let to_string { uid; _ } = uid
end

type choose = Uid.t list -> Uid.t list
type emitters = uid:Uid.t -> string option -> unit
type error = [ Protocol.error | `POP3 of string ]

let pp_error ppf = function
  | #Protocol.error as err -> Protocol.pp_error ppf err
  | `POP3 msg -> Fmt.string ppf msg

let pop3f fmt = Fmt.kstr (fun str -> Protocol.error (`POP3 str)) fmt

let is_ok str =
  if Astring.String.is_prefix ~affix:"+OK" str
  then Protocol.return ()
  else begin
    Log.err (fun m -> m "Invalid POP3 response: %S" str) ;
    pop3f "Invalid POP3 response: %S" str
  end

let entry str =
  match Astring.String.cut ~sep:" " str with
  | Some (sid, uid) -> begin
      match int_of_string sid with
      | sid -> Protocol.return { Uid.sid; uid }
      | exception _ ->
          Log.err (fun m -> m "Invalid UIDL response: %S" str) ;
          pop3f "Invalid UIDL response: %S" str
    end
  | None ->
      Log.err (fun m -> m "Invalid UIDL response: %S" str) ;
      pop3f "Invalid UIDL response: %S" str

let fetch ?authentication ~choose ~emitter_of ctx =
  let* _greeting = Protocol.decode ctx in
  Log.debug (fun m -> m "Greeting: %S" _greeting) ;
  let* () =
    match authentication with
    | Some (username, password) ->
        Log.debug (fun m -> m "authentication state") ;
        let* () = Protocol.encode ctx (Fmt.str "USER %s" username) in
        let* resp = Protocol.decode ctx in
        let* () = is_ok resp in
        let* () = Protocol.encode ctx (Fmt.str "PASS %s" password) in
        let* resp = Protocol.decode ctx in
        is_ok resp
    | None -> Protocol.return () in
  let* () = Protocol.encode ctx "UIDL" in
  let* resp = Protocol.decode ctx in
  let* () = is_ok resp in
  let rec go acc =
    let* str = Protocol.decode ctx in
    match str with
    | "." -> Protocol.return (List.rev acc)
    | str ->
        let* uid = entry str in
        go (uid :: acc) in
  let* lst = go [] in
  let lst = choose lst in
  let rec go = function
    | [] -> Protocol.encode ctx "QUIT"
    | ({ Uid.sid; _ } as uid) :: rest ->
        let* () = Protocol.encode ctx (Fmt.str "RETR %d" sid) in
        let* resp = Protocol.decode ctx in
        let* () = is_ok resp in
        let emitter = emitter_of ~uid in
        let* () = transmit ~emitter ctx in
        let* () = Protocol.encode ctx "NOOP" in
        let* resp = Protocol.decode ctx in
        let* () = is_ok resp in
        go rest in
  go lst
