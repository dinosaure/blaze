let ( let* ) = Protocol.bind

let transmit ~emitter ctx =
  let rec go () =
    let* str = Protocol.decode ctx in
    match str with
    | "." ->
        emitter None ;
        Protocol.return ()
    | str ->
        emitter (Some str) ;
        go () in
  go ()

type uid = { sid : int; uid : string }

let uid { uid; _ } = uid

type choose = uid list -> uid list
type emitters = uid:uid -> string option -> unit

let is_ok str =
  if Astring.String.is_prefix ~affix:"+OK"
  then Protocol.return ()
  else Protocol.error (`POP str)

let fetch ~choose ~emitter_of ctx =
  let* _greeting = Protocol.decode ctx in
  let* () =
    match authentication with
    | Some (username, password) ->
        let* () = Protocol.encode ctx (Fmt.str "USER %s" username) in
        let* resp = Protocol.decode ctx in
        let* () = is_ok resp in
        let* () = Protocol.encode ctx (Fmt.str "PASS %s" password) in
        let* resp = Protocol.decode ctx in
        is_ok resp
    | None -> Protocol.return () in
  let* () = Protocol.encode "UIDL" in
  let* resp = Protocol.decode ctx in
  let* () = is_ok resp in
  let rec go acc =
    let* str = Protocol.decode ctx in
    match str with
    | "." -> Protocol.return (List.rev acc)
    | str ->
        let* uid, id = entry str in
        go ((uid, id) :: acc) in
  let* lst = go [] in
  let lst = choose lst in
  let rec go = function
    | [] -> Protocol.encode ctx "QUIT"
    | ({ sid; _ } as uid) :: rest ->
        let* () = Protocol.encode ctx "RETR %d" sid in
        let* resp = Protocol.decode ctx in
        let* () = is_ok resp in
        let emmitter = emitter_of ~uid in
        let* () = transmit ~emitter ctx in
        go rest in
  go lst
