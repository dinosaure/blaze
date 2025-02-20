let src = Logs.Src.create "blaze.mbox"

module Log = (val Logs.src_log src : Logs.LOG)

let sub str off len =
  if String.length str - off < len then None else Some (String.sub str off len)

let of_in_channel ic =
  let lexbuf = Lexing.from_channel ic in
  let buf = Buffer.create 0x7ff in
  let rec go () =
    let code = Mbox_lexer.token lexbuf in
    match code with
    | 0x500 -> Seq.Nil
    | 0x300 ->
        let line = !Mbox_lexer.to_crlf in
        let lf = String.index line '\n' in
        begin
          match (sub line 0 5, lf == String.length line - 1) with
          | Some "From ", true -> Seq.Cons (`Line line, go)
          | Some "From ", false ->
              let from = String.sub line 0 lf in
              Log.debug (fun m -> m "[0x300] From %s" from) ;
              let line =
                String.sub line (lf + 1) (String.length line - (lf + 1)) in
              let _00 = `From (String.sub from 5 (String.length line - 5)) in
              let _01 = `Line line in
              Seq.(Cons (_00, fun () -> Cons (_01, go)))
          | Some "\nFrom", false ->
              let lf = String.index_from line (lf + 1) '\n' in
              let from = String.sub line 1 (lf - 1) in
              Log.debug (fun m -> m "[0x300] From %s" from) ;
              let line =
                String.sub line (lf + 1) (String.length line - (lf + 1)) in
              let _00 = `From (String.sub from 5 (String.length line - 5)) in
              let _01 = `Line line in
              Seq.(Cons (_00, fun () -> Cons (_01, go)))
          | Some ">From", true ->
              let line = String.sub line 1 (String.length line - 1) in
              Log.debug (fun m -> m "[0x300]-> %S" line) ;
              Seq.Cons (`Line line, go)
          | _ ->
              Log.debug (fun m -> m "[0x300]-> %S" line) ;
              Seq.Cons (`Line line, go)
        end
    | 0x200 ->
        let line = !Mbox_lexer.to_lf in
        begin
          match sub line 0 5 with
          | Some "From " ->
              let from = String.sub line 5 (String.length line - (5 + 1)) in
              Log.debug (fun m -> m "[0x200] From %s" from) ;
              Seq.Cons (`From from, go)
          | Some ">From" ->
              let line = String.sub line 1 (String.length line - (5 + 1)) in
              let line = line ^ "\r\n" in
              Log.debug (fun m -> m "[0x200]-> %S" line) ;
              Seq.Cons (`Line line, go)
          | _ ->
              let line = String.sub line 0 (String.length line - 1) ^ "\r\n" in
              Log.debug (fun m -> m "[0x200]-> %S" line) ;
              Seq.Cons (`Line line, go)
        end
    | 0x100 ->
        let line = Buffer.contents buf in
        Buffer.clear buf ;
        let line = line ^ "\r\n" in
        Log.debug (fun m -> m "[0x100]-> %S" line) ;
        Seq.Cons (`Line line, go)
    | chr ->
        Log.debug (fun m -> m "[0x0??]") ;
        Buffer.add_char buf (Char.chr chr) ;
        go () in
  go
