let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let src = Logs.Src.create "blaze.tree"

module Log = (val Logs.src_log src : Logs.LOG)

type msgid = string
type t = { msgid : msgid; uid : Carton.Uid.t; children : msgid list }

module Format = struct
  open Encore
  open Syntax

  let hex = fixed 20

  let c_string =
    let c_string =
      let fwd str =
        if String.contains str '\000' then raise Bij.Bijection ;
        str in
      let bwd str = str in
      Bij.v ~fwd ~bwd in
    let null =
      let fwd = function "\000" -> () | _ -> raise Bij.Bijection in
      let bwd () = "\000" in
      Bij.v ~fwd ~bwd in
    c_string
    <$> while0 (function '\000' -> false | _ -> true)
    <* (null <$> const "\000")

  let t =
    let tree =
      let bwd { msgid; uid; children } = ((msgid, (uid :> string)), children) in
      let fwd ((msgid, uid), children) =
        { msgid; uid = Carton.Uid.unsafe_of_string uid; children } in
      Bij.v ~fwd ~bwd in
    tree <$> (c_string <*> hex <*> rep0 c_string)
end

module G = Graph.Imperative.Digraph.Concrete (struct
  include Mrmime.MessageID

  let compare a b =
    let v = Emile.compare_local ~case_sensitive:true (fst a) (fst b) in
    if v = 0
    then Emile.compare_domain (snd a :> Emile.domain) (snd b :> Emile.domain)
    else v

  let hash = Hashtbl.hash
end)

let msgid_to_string (local, domain) =
  let pp_elt ppf = function
    | `Atom str -> Fmt.string ppf str
    | `String str ->
        Fmt.pf ppf "\"" ;
        let escape = function
          | '\\' -> Fmt.string ppf "\\\\"
          | '"' -> Fmt.string ppf "\\\""
          | '\000' -> Fmt.string ppf "\\\000"
          | '\x07' -> Fmt.string ppf "\\a"
          | '\b' -> Fmt.string ppf "\\b"
          | '\t' -> Fmt.string ppf "\\t"
          | '\n' -> Fmt.string ppf "\\n"
          | '\x0b' -> Fmt.string ppf "\\v"
          | '\x0c' -> Fmt.string ppf "\\f"
          | '\r' -> Fmt.string ppf "\\r"
          | chr -> Fmt.char ppf chr in
        String.iter escape str ;
        Fmt.pf ppf "\"" in
  let pp_local = Fmt.list ~sep:(Fmt.any ".") pp_elt in
  let pp_domain ppf = function
    | `Domain vs -> Fmt.list ~sep:(Fmt.any ".") Fmt.string ppf vs
    | `Literal str -> Fmt.pf ppf "[%s]" str in
  Fmt.str "<%a@%a>" pp_local local pp_domain domain

let compute emails =
  let graph = G.create () in
  let index = Art.make () in
  let fn (uid_to_email, metadata) =
    match (metadata.Email.in_reply_to, metadata.Email.message_id) with
    | Some x, Some y ->
        let key = Art.key (msgid_to_string y) in
        Log.debug (fun m ->
            m "%a => %a" Mrmime.MessageID.pp x Mrmime.MessageID.pp y) ;
        G.add_edge graph x y ;
        Art.insert index key (uid_to_email, metadata)
    | None, Some x ->
        Log.debug (fun m -> m "=> %a" Mrmime.MessageID.pp x) ;
        let key = Art.key (msgid_to_string x) in
        G.add_vertex graph x ;
        Art.insert index key (uid_to_email, metadata)
    | Some _, _ | None, None -> () in
  Seq.iter fn emails ;
  let fn msgid entries =
    let fn (_, _to) children =
      let _to = msgid_to_string _to in
      _to :: children in
    let key = Art.key (msgid_to_string msgid) in
    match (Art.find_opt index key, G.fold_succ_e fn graph msgid []) with
    | None, _ -> entries
    | _, [] -> entries
    | Some (uid, _), children ->
        { msgid = (key :> string); uid; children } :: entries in
  G.fold_vertex fn graph []

let of_string str =
  let parser = Encore.to_angstrom Format.t in
  match Angstrom.parse_string ~consume:All parser str with
  | Ok t -> Ok t
  | Error _ -> error_msgf "Invalid tree object"
