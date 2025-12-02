type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let display_tree =
  Lazy.from_fun @@ fun () ->
  match Fmt.utf_8 Fmt.stdout with
  | true -> (("┌── ", "│   "), ("├── ", "│   "), ("└── ", "    "), "─┐", "·")
  | false -> ((".-- ", "|   "), ("|-- ", "|   "), ("`-- ", "    "), "-.", "x")

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

module Skeleton = struct
  type transport_padding = string

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

  let rec pp ?(prefix = "") ?(is_last = false) pp_elt ppf tree =
    let _, between, last, expand, none = Lazy.force display_tree in
    let branch, next_prefix = if is_last then last else between in
    match tree with
    | Single None -> Fmt.pf ppf "%s%s%s@." prefix branch none
    | Single (Some v) -> Fmt.pf ppf "%s%s%a@." prefix branch pp_elt v
    | Message { body; _ } ->
        Fmt.pf ppf "%s%s<message>%s@." prefix branch expand ;
        let prefix = prefix ^ next_prefix in
        pp ~prefix ~is_last:true pp_elt ppf body
    | Multipart { parts; _ } ->
        Fmt.pf ppf "%s%s<multipart>%s@." prefix branch expand ;
        let prefix = prefix ^ next_prefix in
        let rec go = function
          | [] -> ()
          | [ (_, { body; _ }) ] -> pp ~prefix ~is_last:true pp_elt ppf body
          | (_, { body; _ }) :: r ->
              pp ~prefix ~is_last:false pp_elt ppf body ;
              go r in
        go parts

  let pp pp_elt ppf = function
    | Single _ as v -> pp pp_elt ppf v
    | Multipart { parts; _ } ->
        let top, _, _, expand, _ = Lazy.force display_tree in
        let branch, next_prefix = top in
        Fmt.pf ppf "%s<multipart>%s@." branch expand ;
        let prefix = next_prefix in
        let rec go = function
          | [] -> ()
          | [ (_, { body; _ }) ] -> pp ~prefix ~is_last:true pp_elt ppf body
          | (_, { body; _ }) :: r ->
              pp ~prefix ~is_last:false pp_elt ppf body ;
              go r in
        go parts
    | Message { body; _ } ->
        let top, _, _, expand, _ = Lazy.force display_tree in
        let branch, next_prefix = top in
        Fmt.pf ppf "%s<message>%s@." branch expand ;
        let prefix = next_prefix in
        pp ~prefix ~is_last:true pp_elt ppf body

  let pp pp_elt ppf ({ body; _ } : 'a t) = pp pp_elt ppf body

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
end

module Semantic = struct
  type 'octet document =
    | Leaf of { mime : string; lang : Snowball.Language.t; contents : 'octet }
    | Choose of { mime : string; parts : 'octet document list }

  and 'octet t = 'octet document option

  let rec map fn = function
    | Leaf { mime; lang; contents; _ } ->
        let contents = fn contents in
        Leaf { mime; lang; contents }
    | Choose { mime; parts } ->
        let fn = map fn in
        let parts = List.map fn parts in
        Choose { mime; parts }

  let rec fold fn acc = function
    | Leaf { mime; lang; contents } -> fn acc (mime, lang, contents)
    | Choose { parts; _ } -> List.fold_left (fold fn) acc parts

  let fold fn acc = Option.fold ~none:acc ~some:(fold fn acc)

  let rec pp ?(prefix = "") ?(is_last = false) pp_elt ppf tree =
    let _, between, last, expand, _ = Lazy.force display_tree in
    let branch, next_prefix = if is_last then last else between in
    match tree with
    | Leaf { contents; mime; _ } ->
        Fmt.pf ppf "%s%s(%s) %a@." prefix branch mime pp_elt contents
    | Choose { mime; parts; _ } ->
        Fmt.pf ppf "%s%s<%s>%s@." prefix branch mime expand ;
        let prefix = prefix ^ next_prefix in
        let rec go = function
          | [] -> ()
          | [ x ] -> pp ~prefix ~is_last:true pp_elt ppf x
          | x :: r ->
              pp ~prefix ~is_last:false pp_elt ppf x ;
              go r in
        go parts

  let pp pp_elt ppf = function
    | Leaf _ as v -> pp pp_elt ppf v
    | Choose { parts; mime; _ } ->
        let top, _, _, expand, _ = Lazy.force display_tree in
        let branch, next_prefix = top in
        Fmt.pf ppf "%s<%s>%s@." branch mime expand ;
        let prefix = next_prefix in
        let rec go = function
          | [] -> ()
          | [ x ] -> pp ~prefix ~is_last:true pp_elt ppf x
          | x :: r ->
              pp ~prefix ~is_last:false pp_elt ppf x ;
              go r in
        go parts

  let pp pp_elt ppf = function
    | None ->
        let _, _, _, _, none = Lazy.force display_tree in
        Fmt.pf ppf "%s\n%!" none
    | Some t -> pp pp_elt ppf t
end

type 'octet t = 'octet Skeleton.t * 'octet Semantic.t

let map fn ((skeleton, semantic) : 'a t) =
  (Skeleton.map fn skeleton, Option.map (Semantic.map fn) semantic)

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
      {
        Skeleton.preamble;
        epilogue = (epilogue, transport_padding);
        boundary;
        parts;
      } in
    let bwd
        {
          Skeleton.preamble;
          epilogue = epilogue, transport_padding;
          boundary;
          parts;
        } =
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
      let fwd () = Skeleton.Single None in
      let bwd = function
        | Skeleton.Single None -> ()
        | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    single_none <$> ctor '\001'

  let single_some =
    let single_some =
      let fwd hex = Skeleton.Single (Some hex) in
      let bwd = function
        | Skeleton.Single (Some hex) -> hex
        | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    single_some <$> ctor '\002' *> hex

  let multipart : 'a Skeleton.part t -> 'a Skeleton.body t =
   fun part ->
    let bijection =
      let fwd multipart = Skeleton.Multipart multipart in
      let bwd = function
        | Skeleton.Multipart multipart -> multipart
        | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    bijection <$> ctor '\003' *> multipart part

  let body : string Skeleton.part t -> string Skeleton.body t =
   fun part ->
    let bijection =
      let fwd t = Skeleton.Message t in
      let bwd = function Skeleton.Message t -> t | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    let message = bijection <$> ctor '\004' *> part in
    choice [ single_none; single_some; multipart part; message ]

  let part =
    let part =
      let fwd (hash, body) = { Skeleton.headers = hash; body } in
      let bwd { Skeleton.headers = hash; body } = (hash, body) in
      Bij.v ~fwd ~bwd in
    fix @@ fun m -> map part (hex <*> body m)

  let lang =
    let fwd str =
      let fn (lang : Snowball.Language.t) = (lang :> string) = str in
      List.find fn Snowball.languages in
    let bwd (lang : Snowball.Language.t) = (lang :> string) in
    Bij.v ~fwd ~bwd <$> c_string

  let leaf =
    let leaf =
      let fwd ((mime, lang), contents) =
        Semantic.Leaf { mime; lang; contents } in
      let bwd = function
        | Semantic.Leaf { mime; lang; contents } -> ((mime, lang), contents)
        | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    leaf <$> ctor '\005' *> (c_string <*> lang <*> hex)

  let choose document =
    let bijection =
      let fwd (mime, parts) = Semantic.Choose { mime; parts } in
      let bwd = function
        | Semantic.Choose { mime; parts } -> (mime, parts)
        | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    (* NOTE(dinosaure): we should be able to use [rep1] instead of [rep0]. But
       we actually failed with empty lists. We should assert that [Choose] as,
       at least, one part. *)
    bijection <$> ctor '\006' *> (c_string <*> rep0 document)

  let document_none =
    let bijection =
      let fwd () = None in
      let bwd = function None -> () | _ -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    bijection <$> ctor '\007'

  let document = fix @@ fun m -> choice [ leaf; choose m ]

  let document_some =
    let bijection =
      let fwd t = Some t in
      let bwd = function Some t -> t | None -> raise Bij.Bijection in
      Bij.v ~fwd ~bwd in
    bijection <$> ctor '\008' *> document

  let t = part <*> choice [ document_none; document_some ]
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
    crlf >>= fun _ ->
    string ("--" ^ boundary) *> transport_padding >>= fun transport_padding ->
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
    option "" (epilogue parent) >>= fun epilogue ->
    return
      {
        Skeleton.preamble;
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
              then Skeleton.Single None
              else Skeleton.Single (Some (start_body, stop_body)) in
            return { Skeleton.headers = (start_hdrs, stop_hdrs); body }
        | None ->
            pos >>= fun start_body ->
            skip_while (Fun.const true) *> pos >>= fun stop_body ->
            let body =
              if start_body == stop_body
              then Skeleton.Single None
              else Skeleton.Single (Some (start_body, stop_body)) in
            return { Skeleton.headers = (start_hdrs, stop_hdrs); body })
    | `Multipart -> (
        match find_boundary hdrs with
        | Some boundary' ->
            multipart ?parent:boundary boundary' (part (Some boundary'))
            >>= fun multipart ->
            return
              {
                Skeleton.headers = (start_hdrs, stop_hdrs);
                body = Multipart multipart;
              }
        | None -> fail "Invalid email: boundary expected")
    | `Message ->
        part boundary >>| fun t ->
        { Skeleton.headers = (start_hdrs, stop_hdrs); body = Message t }

  let t = part None
end

let blit src src_off dst dst_off len =
  Bstr.blit_from_string src ~src_off dst ~dst_off ~len

let g =
  let open Mrmime in
  let open Field_name in
  Map.empty
  |> Map.add date Field.(Witness Unstructured)
  |> Map.add from Field.(Witness Unstructured)
  |> Map.add sender Field.(Witness Unstructured)
  |> Map.add reply_to Field.(Witness Unstructured)
  |> Map.add (v "to") Field.(Witness Unstructured)
  |> Map.add cc Field.(Witness Unstructured)
  |> Map.add bcc Field.(Witness Unstructured)
  |> Map.add subject Field.(Witness Unstructured)
  |> Map.add message_id Field.(Witness Unstructured)
  |> Map.add comments Field.(Witness Unstructured)

let mime_from_headers hdrs =
  let open Mrmime in
  let content_type = Field_name.content_type in
  match Header.assoc content_type hdrs with
  | Field.Field (_, Content, v) :: _ ->
      let text = v.Content_type.ty in
      if text = `Text
      then Some (Content_type.Subtype.to_string v.Content_type.subty)
      else None
  | _ -> None

let to_unstrctrd unstructured =
  let fold acc = function #Unstrctrd.elt as elt -> elt :: acc | _ -> acc in
  let unstrctrd = List.fold_left fold [] unstructured in
  Result.get_ok (Unstrctrd.of_list (List.rev unstrctrd))

let[@ocamlformat "disable"] iso639_2 =
  [ "basque", [ "eus"; "baq/eus"; "eu" ]
  ; "catalan", [ "cat"; "ca" ]
  ; "danish", [ "dan"; "da" ]
  ; "dutch", [ "nld"; "dut/nld"; "nl" ]
  ; "english", [ "eng"; "en" ]
  ; "finnish", [ "fin"; "fi" ]
  ; "french", [ "fra"; "fre/fra"; "fr" ]
  ; "german", [ "deu"; "ger/deu"; "de" ]
  ; "indonesian", [ "ind"; "id" ]
  ; "irish", [ "gle"; "ga" ]
  ; "italian", [ "ita"; "it" ]
  ; "norwegian", [ "nor"; "no" ]
  ; "portuguese", [ "por"; "pt" ]
  ; "spanish", [ "spa"; "es" ]
  ; "swedish", [ "swe"; "sv" ]
  ; "hungarian", [ "hun"; "hu" ]
  ; "russian", [ "rus"; "ru" ]
  ; "arabic", [ "ara"; "ar" ]
  ; "armenian", [ "hye"; "arm/hye"; "hy" ]
  ; "esperanto", [ "epo"; "eo" ]
  ; "estonian", [ "est"; "et" ]
  ; "greek", [ "ell"; "gre/ell" ]
  ; "hindi", [ "hin"; "hi" ]
  ; "lithuanian", [ "lit"; "lt" ]
  ; "nepali", [ "nep"; "ne" ]
  ; "romanian", [ "ron"; "rum/ron"; "ro" ]
  ; "serbian", [ "srp"; "scc/srp"; "sr" ]
  ; "tamil", [ "tam"; "ta" ]
  ; "turkish", [ "tur"; "tr" ]
  ; "yiddish", [ "yid"; "yi" ] ]

let iso639_2 =
  let fn (lang, values) =
    let fn (lang' : Snowball.Language.t) = lang = (lang' :> string) in
    let lang = List.find fn Snowball.languages in
    (lang, values) in
  List.map fn iso639_2

let content_language_to_lang str =
  let langs = String.split_on_char ',' str in
  let langs = List.map String.trim langs in
  let fn value (language, ls) =
    if List.mem value ls then Some language else None in
  let fn acc value =
    match acc with
    | Some _ as acc -> acc
    | None -> List.find_map (fn value) iso639_2 in
  List.fold_left fn None langs

let english =
  let fn ((lang : Snowball.Language.t), _) = (lang :> string) = "english" in
  List.find fn iso639_2 |> fst

let lang_from_headers _lang hdrs =
  let open Mrmime in
  let content_language = Field_name.v "Content-Language" in
  match Header.assoc content_language hdrs with
  | Field.Field (_, Unstructured, v) :: _ ->
      let str = to_unstrctrd v in
      let str = Unstrctrd.fold_fws str in
      let str = Unstrctrd.to_utf_8_string str in
      content_language_to_lang str
  | _ -> Some english

let multipart_from_headers hdrs =
  let open Mrmime in
  let content_type = Field_name.content_type in
  match Header.assoc content_type hdrs with
  | Field.Field (_, Content, v) :: _ ->
      Content_type.Subtype.to_string v.Content_type.subty
  | _ -> "none"

let sink_of_parser parser =
  let init () =
    let ke = Ke.Rke.create ~capacity:0x100 Bigarray.char in
    let state = Angstrom.Unbuffered.parse parser in
    (ke, `Continue state) in
  let push (ke, state) str =
    let open Angstrom.Unbuffered in
    if String.length str > 0
    then
      match state with
      | `Continue (Done (_, v)) -> (ke, `Full v)
      | `Continue (Fail _) -> (ke, `Error)
      | `Continue (Partial { committed; continue }) ->
          Ke.Rke.N.shift_exn ke committed ;
          if committed = 0 then Ke.Rke.compress ke ;
          let length = String.length in
          let len = String.length str in
          Ke.Rke.N.push ke ~blit ~length ~off:0 ~len str ;
          let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
          let off = 0 and len = Bstr.length slice in
          let state = continue slice ~off ~len Incomplete in
          (ke, `Continue state)
      | (`Full _ | `Error) as value -> (ke, value)
    else (ke, state) in
  let full (_ke, state) =
    match state with `Continue _ -> false | `Full _ | `Error -> true in
  let stop (ke, state) =
    let open Angstrom.Unbuffered in
    match state with
    | `Full v | `Continue (Done (_, v)) -> Ok v
    | `Error | `Continue (Fail _) -> Error `Invalid
    | `Continue (Partial { committed; continue }) -> begin
        Ke.Rke.N.shift_exn ke committed ;
        let rem =
          match Ke.Rke.length ke with
          | 0 -> Bstr.empty
          | _ ->
              Ke.Rke.compress ke ;
              List.hd (Ke.Rke.N.peek ke) in
        let off = 0 and len = Bstr.length rem in
        match continue rem ~off ~len Complete with
        | Done (_, v) -> Ok v
        | Fail _ -> Error `Invalid
        | Partial _ -> Error `Not_enough
      end in
  Flux.Sink { init; push; full; stop }

let documents_of_mrmime ?lang (headers, t) =
  let ( let* ) = Option.bind in
  let rec go ~headers = function
    | Mrmime.Mail.Leaf contents ->
        (* select only [text/*] *)
        let* mime = mime_from_headers headers in
        (* select only supported languages *)
        let* lang = lang_from_headers lang headers in
        Some (Semantic.Leaf { mime; lang; contents })
    | Multipart parts ->
        let mime = multipart_from_headers headers in
        let fn acc = function
          | headers, Some contents ->
              (* select only parts with contents *)
              let part = go ~headers contents in
              Option.fold ~none:acc ~some:(fun part -> part :: acc) part
          | _, None -> acc in
        let parts = List.fold_left fn [] parts in
        let parts = List.rev parts in
        Some (Semantic.Choose { mime; parts })
    | Message (headers, t) ->
        let* part = go ~headers t in
        Some (Semantic.Choose { mime = "message"; parts = [ part ] }) in
  go ~headers t

let join a b =
  match (a, b) with
  | Ok a, Ok b -> Ok (a, b)
  | Error err, _ | _, Error err -> Error err

let rec map_from_skeleton skeleton mrmime =
  let open Skeleton in
  let open Mrmime.Mail in
  let ( let* ) = Result.bind in
  match (skeleton, mrmime) with
  | { body = Single (Some contents); _ }, (headers, Leaf ()) ->
      Ok (headers, Leaf contents)
  | { body = Multipart { parts; _ }; _ }, (headers, Multipart parts') -> begin
      try
        let fn (_, skeleton) (headers', part') =
          match (skeleton, part') with
          | { body = Single None; _ }, None -> Ok (headers', None)
          | _, None -> Error `No_symmetry
          | skeleton, Some part' ->
              let* headers', parts' =
                map_from_skeleton skeleton (headers', part') in
              Ok (headers', Some parts') in
        let parts = List.map2 fn parts parts' in
        if List.exists Result.is_error parts
        then Error `No_symmetry
        else Ok (headers, Multipart (List.map Result.get_ok parts))
      with _ -> Error `No_symmetry
    end
  | { body = Message t; _ }, (headers, Message (hdrs', t')) ->
      let* hdrs, t = map_from_skeleton t (hdrs', t') in
      Ok (headers, Message (hdrs, t))
  | _ -> assert false

let of_filename ?lang filename =
  let filename = Fpath.to_string filename in
  let from = Flux.Source.file ~filename 0x7ff in
  let via = Flux.Flow.identity in
  let emitters _hdrs = (Fun.const (), ()) in
  let into =
    let open Flux.Sink.Syntax in
    let+ s = sink_of_parser Parser.t
    and+ m = sink_of_parser (Mrmime.Mail.stream ~g emitters) in
    join s m in
  let result, leftover = Flux.Stream.run ~from ~via ~into in
  Option.iter Flux.Source.dispose leftover ;
  let ( let* ) = Result.bind in
  let* skeleton, mrmime = result in
  let* mrmime = map_from_skeleton skeleton mrmime in
  let documents = documents_of_mrmime ?lang mrmime in
  Ok (skeleton, documents)

let output_bigstring oc bstr =
  let buf = Bytes.create 0x7ff in
  let rec go bstr =
    let dim = Bigarray.Array1.dim bstr in
    if dim > 0
    then begin
      let len = Int.min (Bytes.length buf) dim in
      Bstr.blit_to_bytes bstr ~src_off:0 buf ~dst_off:0 ~len ;
      output_substring oc (Bytes.unsafe_to_string buf) 0 len ;
      go (Bigarray.Array1.sub bstr len (dim - len))
    end in
  go bstr

let rec to_seq ~load t =
  let rec go part =
    let hdr = load part.Skeleton.headers in
    match part.body with
    | Single None -> Seq.return (`Value hdr)
    | Single (Some v) ->
        let body = load v in
        Seq.cons (`Value hdr) (Seq.return (`Value body))
    | Multipart { preamble; epilogue; boundary; parts } ->
        let suffix =
          [ "\r\n"; "--" ^ boundary ^ "--"; fst epilogue; snd epilogue ] in
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
  try Encore.Lavoisier.emit_string ~chunk:0x7ff t emitter
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Log.err (fun m -> m "%s" (Printexc.raw_backtrace_to_string bt)) ;
    raise exn
