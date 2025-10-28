open Mrmime

let rec map_on_edge_headers ~to_headers fn path acc
    { Email.Skeleton.headers; body } =
  let headers = to_headers headers in
  match body with
  | Multipart { parts; _ } ->
      let fn acc (_, t) =
        map_on_edge_headers ~to_headers fn (headers :: path) t acc in
      List.fold_left fn acc parts
  | Single contents -> fn (headers :: path) acc contents
  | Message t -> map_on_edge_headers ~to_headers fn (headers :: path) acc t

let content_type = Field_name.content_type

let is_multipart_mixed = function
  | { Content_type.ty = `Multipart; subty = `Iana_token "mixed"; _ } -> true
  | _ -> false

let is_multipart_mixed hdrs =
  match Header.assoc content_type hdrs with
  | Field.(Field (_, Content, v)) :: _ -> is_multipart_mixed v
  | _ -> false

let is_text hdrs =
  match Header.assoc content_type hdrs with
  | Field.(Field (_, Content, { Content_type.ty = `Text; _ })) :: _ -> true
  | _ -> false

let is_mixed path = List.exists is_multipart_mixed path

type 'contents document =
  | Mixed of (Header.t * 'contents) list
  | Alternated of (Header.t * 'contents) list
  | Parallel of (Header.t * 'contents) list
  | Signed of Header.t * 'contents * 'contents signature
  | Encrypted of Header.t * 'contents * 'contents signature

and 'contents signature = { headers : Mrmime.Header.t; contents : 'contents }

let select path acc contents =
  match path with
  | [] -> assert false
  | [ headers ] -> if is_text headers then contents :: acc else acc
  | headers :: path ->
      if is_mixed path && is_text headers then contents :: acc else acc
