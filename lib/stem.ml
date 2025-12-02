let src = Logs.Src.create "blaze.stem"

module Log = (val Logs.src_log src : Logs.LOG)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

type t = {
  mail : string;
  blob : string;
  length : int;
  tokens : (string, int) Hashtbl.t;
}

module Format = struct
  open Encore
  open Syntax

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

  let int =
    let int =
      let fwd str = String.get_int64_be str 0 |> Int64.to_int in
      let bwd n =
        let buf = Bytes.create 8 in
        Bytes.set_int64_be buf 0 (Int64.of_int n) ;
        Bytes.unsafe_to_string buf in
      Bij.v ~fwd ~bwd in
    int <$> fixed 8

  let hex = fixed 20

  let t =
    let fwd (((uid, blob), length), tokens) =
      let tbl = Hashtbl.create 0x7ff in
      let fn (token, count) = Hashtbl.add tbl token count in
      List.iter fn tokens ;
      (uid, blob, length, tbl) in
    let bwd (uid, blob, length, tokens) =
      let seq = Hashtbl.to_seq tokens in
      (((uid, blob), length), List.of_seq seq) in
    Bij.v ~fwd ~bwd
    <$> (hex <*> hex <*> int <*> rep0 (c_string <*> int <* commit))
end

let to_string t =
  let emitter = Encore.to_lavoisier Format.t in
  try Encore.Lavoisier.emit_string ~chunk:0x7ff t emitter
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Log.err (fun m -> m "%s" (Printexc.raw_backtrace_to_string bt)) ;
    raise exn

let of_string str =
  let parser = Encore.to_angstrom Format.t in
  match Angstrom.parse_string ~consume:All parser str with
  | Ok t -> Ok t
  | Error _ -> error_msgf "Invalid stem object"

let to_length_and_hash ((_, _, _, tbl) as t) =
  let emitter = Encore.to_lavoisier Format.t in
  let length =
    Hashtbl.fold (fun str _ acc -> String.length str + 1 + 8 + acc) tbl 0
    |> ( + ) 20
    |> ( + ) 20
    |> ( + ) 8 in
  let rec go length' ctx = function
    | Encore.Lavoisier.Fail -> failwith "Stem.to_length_and_hash"
    | Done ->
        if length <> length' then failwith "Stem.to_length_and_hash" ;
        (length, Digestif.SHA1.(to_raw_string (get ctx)))
    | Partial { buffer; off; len; continue } ->
        let ctx = Digestif.SHA1.feed_string ctx buffer ~off ~len in
        let length' = length' + len in
        go length' ctx (continue ~committed:len) in
  let ctx = Digestif.SHA1.empty in
  let hdr = Fmt.str "stem %d\000" length in
  let ctx = Digestif.SHA1.feed_string ctx hdr in
  go 0 ctx (Encore.Lavoisier.emit t emitter)
