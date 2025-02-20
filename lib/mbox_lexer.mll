{
  let from = ref ""
  let to_lf = ref ""
  let to_crlf = ref ""
}

rule token = parse
  (* | "From " ([^ '\n']* as str) "\n" { from := str; 0x200 } *)
  | ([^ '\r' '\n' ]* as v) "\r\n" { to_crlf := v ^ "\r\n"; 0x300 }
  | ([^ '\r' '\n' ]* as v) "\n" { to_lf := v ^ "\n"; 0x200 }
  | "\r\n" { 0x100 }
  | _ as chr { Char.code chr }
  | eof { 0x500 }
