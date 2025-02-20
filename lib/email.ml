(* multipart-body :=
      [preamble CRLF]
      --boundary transport-padding CRLF
      part
      ( CRLF
        --boundary transport-padding CRLF
        part )*
      CRLF
      --boundary-- transport-padding
      [CRLF epilogue]

   part :=
     headers
     ( CRLF body )?
*)

let src = Logs.Src.create "blaze.email"

module Log = (val Logs.src_log src : Logs.LOG)

type transport_padding = string

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'octet body =
  | Multipart of 'octet multipart
  | Single of 'octet option
  | Message of 'octet t

and 'octet part = { headers : 'octet; body : 'octet body }

and 'octet multipart = {
  preamble : string;
  epilogue : string * transport_padding;
  boundary : string;
  parts : (transport_padding * 'octet part) list;
}

and 'octet t = 'octet part

let rec map fn { headers; body; _ } =
  let headers = fn headers in
  let body =
    match body with
    | Single None -> Single None
    | Single (Some v) -> Single (Some (fn v))
    | Multipart vs ->
        let fn (transport_padding, part) = (transport_padding, map fn part) in
        let parts = List.map fn vs.parts in
        Multipart { vs with parts }
    | Message t -> Message (map fn t) in
  { headers; body }

let rec fold fn acc { headers; body } =
  let acc = fn acc headers in
  match body with
  | Single None -> acc
  | Single (Some v) -> fn acc v
  | Multipart vs ->
      let fn acc (_, part) = fold fn acc part in
      List.fold_left fn acc vs.parts
  | Message t -> fold fn acc t

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
  (* TODO(dinosaure): we assume that [preamble] and [epilogue] don't contain ['\000'].
     Try to handle this case (which seems accepted by the RFC 2046) is really hard! *)

  let multipart =
    let fwd ((((preamble, epilogue), transport_padding), boundary), parts) =
      { preamble; epilogue = (epilogue, transport_padding); boundary; parts }
    in
    let bwd
        { preamble; epilogue = epilogue, transport_padding; boundary; parts } =
      ((((preamble, epilogue), transport_padding), boundary), parts) in
    Bij.v ~fwd ~bwd

  let multipart part =
    multipart
    <$> (c_string
        <*> c_string
        <*> c_string
        <*> c_string
        <*> rep0 (c_string <*> part))

  let ctor chr =
    let ctor =
      let fwd str =
        if String.length str != 1 || str.[0] != chr then raise Bij.Bijection
      in
      let bwd _value = String.make 1 chr in
      Bij.v ~fwd ~bwd in
    ctor <$> const (String.make 1 chr)

  let single_none =
    let single_none =
      let fwd () = Single None in
      let bwd = function Single None -> () | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    single_none <$> ctor '\001'

  let single_some =
    let single_some =
      let fwd hex = Single (Some hex) in
      let bwd = function Single (Some hex) -> hex | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    single_some <$> ctor '\002' *> hex

  let multipart : 'a part t -> 'a body t =
   fun part ->
    let bijection =
      let fwd multipart = Multipart multipart in
      let bwd = function
        | Multipart multipart -> multipart
        | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    bijection <$> ctor '\003' *> multipart part

  let body : string part t -> string body t =
   fun part ->
    let bijection =
      let fwd t = Message t in
      let bwd = function Message t -> t | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    let message = bijection <$> ctor '\004' *> part in
    choice [ single_none; single_some; multipart part; message ]

  let part =
    let part =
      let fwd (hash, body) = { headers = hash; body } in
      let bwd { headers = hash; body } = (hash, body) in
      Bij.v ~fwd ~bwd in
    fix @@ fun m -> map part (hex <*> body m)

  let t = part
end

module Parser = struct
  let crlf = Angstrom.string "\r\n"

  let transport_padding =
    Angstrom.take_while @@ function '\x09' | '\x20' -> true | _ -> false

  let to_dash_boundary boundary =
    let open Angstrom in
    let dash_boundary = "--" ^ boundary in
    fix @@ fun m ->
    take_while (fun chr -> chr != '-') >>= fun to_dash ->
    peek_char >>= function
    | None -> return [ to_dash ]
    | Some '-' ->
        let len = String.length dash_boundary in
        peek_string len >>= fun str ->
        if dash_boundary = str
        then return [ to_dash ]
        else advance 1 *> m >>= fun sstr -> return (to_dash :: "-" :: sstr)
    | Some chr ->
        advance 1 *> m >>= fun sstr ->
        return (to_dash :: String.make 1 chr :: sstr)

  let skip_until_delimiter boundary =
    let open Angstrom in
    let delimiter = "\r\n--" ^ boundary in
    fix @@ fun m ->
    skip_while (fun chr -> chr != '\r') *> peek_string (String.length delimiter)
    >>= fun str -> if delimiter = str then return () else advance 1 *> m

  let to_delimiter boundary =
    let open Angstrom in
    let delimiter = "\r\n--" ^ boundary in
    fix @@ fun m ->
    take_while (fun chr -> chr != '\r') >>= fun to_delimiter ->
    peek_char >>= function
    | None -> return [ to_delimiter ]
    | Some '\r' ->
        let len = String.length delimiter in
        peek_string len >>= fun str ->
        if delimiter = str
        then return [ to_delimiter ]
        else advance 1 *> m >>= fun sstr -> return (to_delimiter :: "\r" :: sstr)
    | Some chr ->
        advance 1 *> m >>= fun sstr ->
        return (to_delimiter :: String.make 1 chr :: sstr)

  let encapsulation boundary octet =
    let open Angstrom in
    crlf *> string ("--" ^ boundary) *> transport_padding
    >>= fun transport_padding ->
    crlf *> commit *> octet >>= fun part -> return (transport_padding, part)

  let epilogue = function
    | Some boundary ->
        let open Angstrom in
        to_delimiter boundary >>| String.concat ""
    | None -> Angstrom.take_while (Fun.const true)

  let multipart ?parent boundary octet =
    let open Angstrom in
    to_dash_boundary boundary >>| String.concat "" >>= fun preamble ->
    string ("--" ^ boundary) *> transport_padding >>= fun transport_padding0 ->
    crlf *> commit *> octet >>= fun part ->
    many (encapsulation boundary octet) >>= fun r ->
    crlf *> commit *> string ("--" ^ boundary ^ "--") *> transport_padding
    >>= fun transport_padding1 ->
    option "" (crlf *> epilogue parent) >>= fun epilogue ->
    return
      {
        preamble;
        epilogue = (epilogue, transport_padding1);
        boundary;
        parts = (transport_padding0, part) :: r;
      }

  let find_boundary hdrs =
    let open Mrmime in
    let content_type = Header.content_type hdrs in
    Content_type.boundary content_type

  let rec part boundary =
    let open Angstrom in
    let open Mrmime in
    pos >>= fun start_hdrs ->
    Header.Decoder.header None >>= fun hdrs ->
    pos >>= fun stop_hdrs ->
    commit
    *>
    match Content_type.ty (Header.content_type hdrs) with
    | `Ietf_token _ | `X_token _ | #Content_type.Type.discrete -> (
        match boundary with
        | Some boundary ->
            pos >>= fun start_body ->
            skip_until_delimiter boundary *> pos >>= fun stop_body ->
            let body =
              if start_body == stop_body
              then Single None
              else Single (Some (start_body, stop_body)) in
            return { headers = (start_hdrs, stop_hdrs); body }
        | None ->
            pos >>= fun start_body ->
            skip_while (Fun.const true) *> pos >>= fun stop_body ->
            let body =
              if start_body == stop_body
              then Single None
              else Single (Some (start_body, stop_body)) in
            return { headers = (start_hdrs, stop_hdrs); body })
    | `Multipart -> (
        match find_boundary hdrs with
        | Some boundary' ->
            multipart ?parent:boundary boundary' (part (Some boundary'))
            >>= fun multipart ->
            return
              { headers = (start_hdrs, stop_hdrs); body = Multipart multipart }
        | None -> fail "Invalid email: boundary expected")
    | `Message ->
        part boundary >>| fun t ->
        { headers = (start_hdrs, stop_hdrs); body = Message t }

  let t = part None
end

external bigstring_set_uint8 : bigstring -> int -> int -> unit
  = "%caml_ba_set_1"

external bigstring_set_int32_ne : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

let bigstring_blit_from_bytes src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len lsr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = Bytes.get_int32_ne src (src_off + i) in
    bigstring_set_int32_ne dst (dst_off + i) v
  done ;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = Bytes.get_uint8 src (src_off + i) in
    bigstring_set_uint8 dst (dst_off + i) v
  done

let bigstring_blit_from_string src src_off dst dst_off len =
  bigstring_blit_from_bytes
    (Bytes.unsafe_of_string src)
    ~src_off dst ~dst_off ~len

external bigstring_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"

external bigstring_get_int32_ne : bigstring -> int -> int32
  = "%caml_bigstring_get32"

let bigstring_blit_to_bytes src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len lsr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = bigstring_get_int32_ne src (src_off + i) in
    Bytes.set_int32_ne dst (dst_off + i) v
  done ;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = bigstring_get_uint8 src (src_off + i) in
    Bytes.set_uint8 dst (dst_off + i) v
  done

let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let of_filename filename =
  let ic = open_in (Fpath.to_string filename) in
  let buf = Bytes.create 0x7ff in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let rec go = function
    | Angstrom.Unbuffered.Partial { committed; continue } -> (
        Ke.Rke.N.shift_exn ke committed ;
        Ke.Rke.compress ke ;
        match input ic buf 0 (Bytes.length buf) with
        | 0 | (exception End_of_file) ->
            let buf =
              match Ke.Rke.length ke with
              | 0 -> bigstring_empty
              | _ ->
                  Ke.Rke.compress ke ;
                  List.hd (Ke.Rke.N.peek ke) in
            let len = Bigarray.Array1.dim buf in
            let state = continue buf ~off:0 ~len Complete in
            go state
        | len ->
            Ke.Rke.N.push ke ~blit:bigstring_blit_from_string
              ~length:String.length ~len
              (Bytes.unsafe_to_string buf) ;
            let buf = List.hd (Ke.Rke.N.peek ke) in
            let len = Bigarray.Array1.dim buf in
            let state = continue buf ~off:0 ~len Incomplete in
            go state)
    | Done (_, t) -> Ok t
    | Fail _ -> error_msgf "Invalid email" in
  go (Angstrom.Unbuffered.parse Parser.t)

let output_bigstring oc bstr =
  let buf = Bytes.create 0x7ff in
  let rec go bstr =
    let dim = Bigarray.Array1.dim bstr in
    if dim > 0
    then (
      let len = Int.min (Bytes.length buf) dim in
      bigstring_blit_to_bytes bstr ~src_off:0 buf ~dst_off:0 ~len ;
      output_substring oc (Bytes.unsafe_to_string buf) 0 len ;
      go (Bigarray.Array1.sub bstr len (dim - len))) in
  go bstr

let rec to_seq ~load t =
  let rec go part =
    let hdr = load part.headers in
    match part.body with
    | Single None -> Seq.return (`Value hdr)
    | Single (Some v) ->
        let body = load v in
        Seq.cons (`Value hdr) (Seq.return (`Value body))
    | Multipart { preamble; epilogue; boundary; parts } ->
        let suffix =
          [ "\r\n"; "--" ^ boundary ^ "--"; fst epilogue; snd epilogue; "\r\n" ]
        in
        let suffix = List.to_seq suffix in
        let suffix = Seq.map (fun str -> `String str) suffix in
        let parts = List.to_seq parts in
        let first = ref true in
        let fn (transport_padding, part) =
          let prefix = [ "--" ^ boundary; transport_padding; "\r\n" ] in
          let prefix = if not !first then "\r\n" :: prefix else prefix in
          first := false ;
          let prefix = List.map (fun str -> `String str) prefix in
          List.fold_right Seq.cons prefix (go part) in
        let parts = Seq.map fn parts in
        let lst =
          [
            Seq.(return (return (`Value hdr)));
            Seq.(return (return (`String preamble)));
            parts;
            Seq.return suffix;
          ] in
        let seq = List.to_seq lst in
        Seq.(concat (concat seq))
    | Message t -> Seq.cons (`Value hdr) (to_seq ~load t) in
  go t

let to_output_channel_from_filename filename t oc =
  let fd = Unix.openfile (Fpath.to_string filename) Unix.[ O_RDONLY ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let map ~off ~len =
    Log.debug (fun m -> m "map off:%08x len:%d" off len) ;
    let barr =
      Unix.map_file fd ~pos:(Int64.of_int off) Bigarray.char Bigarray.c_layout
        false [| len |] in
    Bigarray.array1_of_genarray barr in
  let load (pos, pos_end) = map ~off:pos ~len:(pos_end - pos) in
  let seq = to_seq ~load t in
  let fn = function
    | `String str -> output_string oc str
    | `Value bstr -> output_bigstring oc bstr in
  Seq.iter fn seq

(*
let to_output_channel_from_filename filename t oc =
  let fd = Unix.openfile (Fpath.to_string filename) Unix.[ O_RDONLY ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let map ~off ~len =
    Log.debug (fun m -> m "map off:%08x len:%d" off len) ;
    let barr =
      Unix.map_file fd ~pos:(Int64.of_int off) Bigarray.char Bigarray.c_layout
        false [| len |] in
    Bigarray.array1_of_genarray barr in
  let rec go part =
    let pos, end_pos = part.headers in
    let bstr = map ~off:pos ~len:(end_pos - pos) in
    output_bigstring oc bstr ;
    match part.body with
    | Single None -> ()
    | Single (Some (pos, end_pos)) ->
        let bstr = map ~off:pos ~len:(end_pos - pos) in
        output_bigstring oc bstr
    | Multipart { preamble; epilogue; boundary; parts } ->
        output_string oc preamble ;
        let first = ref true in
        let fn (transport_padding, part) =
          if not !first then output_string oc "\r\n" else first := false ;
          output_string oc ("--" ^ boundary) ;
          output_string oc transport_padding ;
          output_string oc "\r\n" ;
          go part in
        List.iter fn parts ;
        output_string oc "\r\n" ;
        output_string oc ("--" ^ boundary ^ "--") ;
        output_string oc (fst epilogue) ;
        output_string oc (snd epilogue) ;
        output_string oc "\r\n" in
  go t
*)

let of_string str =
  let parser = Encore.to_angstrom Format.t in
  match Angstrom.parse_string ~consume:All parser str with
  | Ok t -> Ok t
  | Error _ -> error_msgf "Invalid object"

let of_bigstring bstr =
  let parser = Encore.to_angstrom Format.t in
  match Angstrom.parse_bigstring ~consume:All parser bstr with
  | Ok t -> Ok t
  | Error _ -> error_msgf "Invalid object"

let to_string t =
  let emitter = Encore.to_lavoisier Format.t in
  Encore.Lavoisier.emit_string ~chunk:0x7ff t emitter
