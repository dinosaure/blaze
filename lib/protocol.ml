let src = Logs.Src.create "protocol"

module Log = (val Logs.src_log src : Logs.LOG)

module Decoder = struct
  type t = { buffer : bytes; mutable pos : int; mutable max : int }

  let make len = { buffer = Bytes.make len '\000'; pos = 0; max = 0 }

  let leftover { buffer; pos; max } =
    let len = pos - max in
    Bytes.sub_string buffer pos len

  type ('v, 'err) state =
    | Done of 'v
    | Read of {
        buffer : bytes;
        off : int;
        len : int;
        continue : ('v, 'err) continue;
      }
    | Error of 'err info

  and ('v, 'err) continue = [ `End | `Len of int ] -> ('v, 'err) state
  and 'err info = { error : 'err; buffer : bytes; committed : int }

  type error =
    [ `End_of_input | `Not_enough_space | `Expected_eol | `Invalid_pkt_line ]

  let pp_error ppf = function
    | `End_of_input -> Fmt.string ppf "End of input"
    | `Not_enough_space -> Fmt.string ppf "Not enough space"
    | `Expected_eol -> Fmt.string ppf "Expected EOL"
    | `Invalid_pkt_line -> Fmt.string ppf "Invalid PKT-line"

  exception Leave of error info

  let return v _ = Done v
  let end_of_input t = t.max

  let safe k decoder =
    try k decoder with Leave ({ error = #error; _ } as info) -> Error info

  let leave_with (t : t) error : 'a =
    let info = { error; buffer = t.buffer; committed = t.pos } in
    raise (Leave info)

  (* NOTE(dinosaure): for POP3. *)
  let at_least_one_line (t : t) =
    let pos = ref t.pos in
    let chr = ref '\000' in
    let has_cr = ref false in
    while
      !pos < t.max
      &&
      (chr := Bytes.unsafe_get t.buffer !pos ;
       not (!chr = '\n' && !has_cr))
    do
      has_cr := !chr = '\r' ;
      incr pos
    done ;
    !pos < t.max && !chr = '\n' && !has_cr

  let hex t str =
    let to_int = function
      | 'a' .. 'f' as chr -> 10 + Char.code chr - Char.code 'a'
      | 'A' .. 'F' as chr -> 10 + Char.code chr - Char.code 'A'
      | '0' .. '9' as chr -> Char.code chr - Char.code '0'
      | _ -> leave_with t `Invalid_pkt_line in
    let v = to_int str.[0] in
    let v = (16 * v) + to_int str.[1] in
    let v = (16 * v) + to_int str.[2] in
    (16 * v) + to_int str.[3]

  (* NOTE(dinosaure): for Git. *)
  let at_least_one_pkt (t : t) =
    let len = t.max - t.pos in
    if len >= 4 then hex t (Bytes.sub_string t.buffer t.pos 4) <= len else false

  let prompt ~at_least k decoder =
    if decoder.pos > 0
    then begin
      let rest = decoder.max - decoder.pos in
      Bytes.unsafe_blit decoder.buffer decoder.pos decoder.buffer 0 rest ;
      decoder.max <- rest ;
      decoder.pos <- 0
    end ;
    let rec go off =
      let at_least_something = at_least { decoder with max = off } in
      if (not at_least_something) && off = Bytes.length decoder.buffer
      then
        let info =
          {
            error = `Not_enough_space;
            buffer = decoder.buffer;
            committed = decoder.pos;
          } in
        Error info
      else if not at_least_something
      then
        let continue = function
          | `Len len -> go (off + len)
          | `End ->
              let info =
                {
                  error = `End_of_input;
                  buffer = decoder.buffer;
                  committed = decoder.pos;
                } in
              Error info in
        Read
          {
            buffer = decoder.buffer;
            off;
            len = Bytes.length decoder.buffer - off;
            continue;
          }
      else begin
        decoder.max <- off ;
        safe k decoder
      end in
    go decoder.max

  let peek_pkt (t : t) =
    if t.max - t.pos < 4 then leave_with t `Invalid_pkt_line ;
    let len = hex t (Bytes.sub_string t.buffer t.pos 4) in
    if t.max - t.pos < len then leave_with t `Invalid_pkt_line ;
    (t.buffer, t.pos + 4, Int.max 0 (len - 4))

  let peek_while_eol t =
    let idx = ref t.pos in
    let chr = ref '\000' in
    let has_cr = ref false in
    while
      !idx < end_of_input t
      &&
      (chr := Bytes.unsafe_get t.buffer !idx ;
       not (!chr = '\n' && !has_cr))
    do
      has_cr := !chr = '\r' ;
      incr idx
    done ;
    if !idx < end_of_input t && !chr = '\n' && !has_cr
    then (t.buffer, t.pos, !idx + 1 - t.pos)
    else leave_with t `Expected_eol

  let skip t len = t.pos <- t.pos + len
end

module Encoder = struct
  type t = { payload : bytes; mutable pos : int }

  let make len = { payload = Bytes.make len '\000'; pos = 0 }

  type 'err state =
    | Write of {
        buffer : string;
        off : int;
        len : int;
        continue : 'err continue;
      }
    | Error of 'err
    | Done

  and 'err continue = int -> 'err state

  type error = [ `Not_enough_space ]

  let pp_error ppf = function
    | `Not_enough_space -> Fmt.string ppf "Not enough space"

  exception Leave of error

  let leave_with _ error = raise (Leave error)
  let safe k encoder = try k encoder with Leave (#error as err) -> Error err

  let flush k0 encoder =
    if encoder.pos > 0
    then
      let rec k1 n =
        if n < encoder.pos
        then
          Write
            {
              buffer = Bytes.unsafe_to_string encoder.payload;
              off = n;
              len = encoder.pos - n;
              continue = (fun m -> k1 (n + m));
            }
        else begin
          encoder.pos <- 0 ;
          k0 encoder
        end in
      k1 0
    else k0 encoder

  let write str encoder =
    let max = Bytes.length encoder.payload in
    let go j l encoder =
      let rem = max - encoder.pos in
      let len = if l > rem then rem else l in
      Bytes.blit_string str j encoder.payload encoder.pos len ;
      encoder.pos <- encoder.pos + len ;
      if len < l then leave_with encoder `Not_enough_space in
    go 0 (String.length str) encoder

  let blit src ~off ~len encoder =
    let max = Bytes.length encoder.payload in
    let go j l encoder =
      let rem = max - encoder.pos in
      let len = if l > rem then rem else l in
      Bytes.blit_string src (off + j) encoder.payload encoder.pos len ;
      if len < l then leave_with encoder `Not_enough_space in
    go 0 len encoder
end

type (+'a, 'err) t =
  | Read of {
      buffer : bytes;
      off : int;
      len : int;
      k : [ `End | `Len of int ] -> ('a, 'err) t;
    }
  | Write of { buffer : string; off : int; len : int; k : int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err

let ( % ) f g = fun x -> f (g x)

let rec reword_error fn = function
  | Error err -> Error (fn err)
  | Read { k; buffer; off; len } ->
      Read { k = reword_error fn % k; buffer; off; len }
  | Write { k; buffer; off; len } ->
      Write { k = reword_error fn % k; buffer; off; len }
  | Return _ as v -> v

let rec go ~fn t len =
  match t len with
  | Return v -> fn v
  | Read { k; buffer; off; len } -> Read { k = go ~fn k; buffer; off; len }
  | Write { k; buffer; off; len } ->
      let k0 = function `End -> k 0 | `Len len -> k len in
      let k1 = function 0 -> go ~fn k0 `End | len -> go ~fn k0 (`Len len) in
      Write { k = k1; buffer; off; len }
  | Error err -> Error err

let bind t fn =
  match t with
  | Return v -> fn v
  | Error err -> Error err
  | Read { k; buffer; off; len } -> Read { k = go ~fn k; buffer; off; len }
  | Write { k; buffer; off; len } ->
      let k0 = function `End -> k 0 | `Len len -> k len in
      let k1 = function 0 -> go ~fn k0 `End | len -> go ~fn k0 (`Len len) in
      Write { k = k1; buffer; off; len }

let return v = Return v
let error err = Error err

type ctx = { encoder : Encoder.t; decoder : Decoder.t }
type error = [ Decoder.error | Encoder.error ]

let pp_error ppf = function
  | #Encoder.error as err -> Encoder.pp_error ppf err
  | #Decoder.error as err -> Decoder.pp_error ppf err

let ctx () = { encoder = Encoder.make 65536; decoder = Decoder.make 65536 }
let leftover ctx = Decoder.leftover ctx.decoder

let encode_str ctx str =
  let k t =
    Encoder.write str t ;
    Encoder.flush (fun _t -> Encoder.Done) t in
  let k t = Encoder.safe k t in
  let rec go = function
    | Encoder.Done -> Return ()
    | Encoder.Write { buffer; off; len; continue } ->
        Write { k = go % continue; buffer; off; len }
    | Encoder.Error err -> Error err in
  go (k ctx.encoder)

let encode_line ctx str = encode_str ctx (str ^ "\r\n")

let hex =
  let[@ocamlformat "disable"] to_char =
    [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
       'A'; 'B'; 'C'; 'D'; 'E'; 'F'; |] in
  fun n ->
    if n < 0 then invalid_arg "Protocol.hex: negative number" ;
    if n > 65535 then Fmt.invalid_arg "Protocol.hex: too large number (%d)" n ;
    let n = ref n in
    let buf = Bytes.create 4 in
    Bytes.set buf 3 to_char.(!n mod 16) ;
    n := !n / 16 ;
    Bytes.set buf 2 to_char.(!n mod 16) ;
    n := !n / 16 ;
    Bytes.set buf 1 to_char.(!n mod 16) ;
    n := !n / 16 ;
    Bytes.set buf 0 to_char.(!n) ;
    Bytes.unsafe_to_string buf

let encode_pkt ctx str =
  let hdr = hex (String.length str + 4) in
  encode_str ctx (hdr ^ str)

let encode_flush_pkt ctx = encode_str ctx "0000"
let encode_delim_pkt ctx = encode_str ctx "0001"

type ('r, 'err) fmt =
  ('r, Format.formatter, unit, (unit, ([> Encoder.error ] as 'err)) t) format4

let encode_pkt ctx fmt = Fmt.kstr (encode_pkt ctx) fmt

let decode_line ctx =
  let at_least = Decoder.at_least_one_line in
  let k t =
    let buf, off, len = Decoder.peek_while_eol t in
    let str = Bytes.sub_string buf off (len - 2) in
    Decoder.skip t len ;
    Decoder.Done str in
  let k t =
    if at_least t then Decoder.safe k t else Decoder.prompt ~at_least k t in
  let rec go = function
    | Decoder.Done v -> Return v
    | Decoder.Read { buffer; off; len; continue } ->
        Read { k = Fun.compose go continue; buffer; off; len }
    | Decoder.Error { error; _ } -> Error error in
  go (k ctx.decoder)

let decode_pkt ctx =
  let at_least = Decoder.at_least_one_pkt in
  let k t =
    let buf, off, len = Decoder.peek_pkt t in
    let str = Bytes.sub_string buf off len in
    Decoder.skip t (4 + len) ;
    Decoder.Done str in
  let k t =
    if at_least t then Decoder.safe k t else Decoder.prompt ~at_least k t in
  let rec go = function
    | Decoder.Done v -> Return v
    | Decoder.Read { buffer; off; len; continue } ->
        Read { k = Fun.compose go continue; buffer; off; len }
    | Decoder.Error { error; _ } -> Error error in
  go (k ctx.decoder)
