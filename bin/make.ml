open Mrmime
open Rresult

let add_unless_exists field_name (field, v) hdrs =
  let open Mrmime in
  if Header.exists field_name hdrs
  then hdrs else Header.add field_name (field, v) hdrs

let run
  headers
  content_encoding
  content_type 
  from
  _to
  cc
  bcc
  zone with_date
  body =
  let hdrs =
    headers
    |> add_unless_exists Field_name.content_type Field.(Content, content_type)
    |> add_unless_exists Field_name.content_encoding Field.(Encoding, content_encoding) in
  let hdrs = match from with
    | Some (sender :: mailboxes) ->
      hdrs
      |> add_unless_exists Field_name.from Field.(Mailboxes, sender :: mailboxes)
      |> add_unless_exists Field_name.sender Field.(Mailbox, sender)
    | Some [] | None -> hdrs in
  let hdrs = match _to with
    | Some addresses -> add_unless_exists Field_name.(v "To") Field.(Addresses, addresses) hdrs
    | None -> hdrs in
  let hdrs = match cc with
    | Some addresses -> Header.add Field_name.cc Field.(Addresses, addresses) hdrs
    | None -> hdrs in
  let hdrs = match bcc with
    | Some addresses -> Header.add Field_name.bcc Field.(Addresses, addresses) hdrs
    | None -> hdrs in
  let hdrs = match with_date with
    | `Specific ptime ->
      let date = Date.of_ptime ~zone ptime in
      Header.add Field_name.date Field.(Date, date) hdrs
    | `Now ->
      let date = Date.of_ptime ~zone (Ptime_clock.now ()) in
      Header.add Field_name.date Field.(Date, date) hdrs
    | `None -> hdrs in 
  let part = Mt.part ~header:hdrs body in
  let mail = Mt.make Header.empty Mt.simple part in
  let _stream = Mt.to_stream mail in
  assert false

open Cmdliner

let field =
  let parser str = match String.split_on_char ':' str with
    | [ field_name; value ] ->
      ( match Field_name.of_string field_name, Unstructured.of_string value with
      | Ok field_name, Ok unstrctrd ->
        Ok (Field.make field_name Field.Unstructured (unstrctrd :> Unstructured.elt list))
      | Error _, _ -> R.error_msgf "Invalid field-name: %S" field_name
      | _, Error _ -> R.error_msgf "Invalid unstructred value: %S" value )
    | _ -> R.error_msgf "Invalid field: %S" str in
  let pp ppf (Field.Field (field_name, w, v)) = match w with
    | Field.Unstructured -> Fmt.pf ppf "%a:%a" Field_name.pp field_name Unstructured.pp v
    | _ -> assert false in
  Arg.conv (parser, pp)

let content_encoding =
  Arg.conv (Content_encoding.of_string, Content_encoding.pp)
