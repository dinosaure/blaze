open Mrmime
open Rresult

let identity x = x

let default =
  let open Field_name in
  Map.empty
  |> Map.add from Field.(Witness Mailboxes)
  |> Map.add (v "to") Field.(Witness Addresses)
  |> Map.add cc Field.(Witness Addresses)
  |> Map.add bcc Field.(Witness Addresses)
  |> Map.add sender Field.(Witness Mailbox)
  |> Map.add date Field.(Witness Date)
  |> Map.add subject Field.(Witness Unstructured)
  |> Map.add message_id Field.(Witness MessageID)
  |> Map.add comments Field.(Witness Unstructured)
  |> Map.add content_type Field.(Witness Content)
  |> Map.add content_encoding Field.(Witness Encoding)

let decode_rfc2047 = ref false

let pp_encoded ~charset ppf = function
  | Emile.Quoted_printable (Ok v) ->
      let buf = Buffer.create (String.length v) in
      let encoder = Pecu.Inline.encoder (`Buffer buf) in
      let rec go idx =
        let[@warning "-8"] (`Ok : [ `Ok | `Partial ]) =
          if idx = String.length v
          then Pecu.Inline.encode encoder `End
          else Pecu.Inline.encode encoder (`Char v.[idx]) in
        if idx < String.length v then go (succ idx) in
      go 0 ;
      Fmt.pf ppf "=?%s?Q?%s?=" charset (Buffer.contents buf)
  | Emile.Base64 (Ok v) ->
      Fmt.pf ppf "=?%s?B?%s?=" charset (Base64.encode_exn ~pad:true v)
  | _ -> assert false

let pp_phrase ppf phrase =
  let pp_elem ppf = function
    | `Dot -> Fmt.string ppf "."
    | `Word (`Atom x) -> Fmt.string ppf x
    | `Word (`String x) -> Fmt.(quote string) ppf x
    | `Encoded (_, Emile.Quoted_printable (Ok v)) when !decode_rfc2047 ->
        Fmt.string ppf v
    | `Encoded (_, Emile.Base64 (Ok v)) when !decode_rfc2047 -> Fmt.string ppf v
    | `Encoded (charset, v) -> pp_encoded ~charset ppf v in
  Fmt.(list ~sep:(any "@ ") pp_elem) ppf phrase

let pp_mailbox ppf = function
  | { Emile.name = None; _ } as v -> Emile.pp_mailbox ppf v
  | { name = Some name; domain = _, []; _ } as v ->
      Fmt.pf ppf "@[<hov>%a@] <%a>" pp_phrase name Emile.pp_mailbox
        { v with Emile.name = None }
  | { name = Some name; _ } as v ->
      (* XXX(dinosaure): with multiple domains, we know that [emile] surrounds with "<" and ">". *)
      Fmt.pf ppf "@[<hov>%a@] %a" pp_phrase name Emile.pp_mailbox
        { v with Emile.name = None }

let pp_group ppf { Emile.group; mailboxes } =
  Fmt.pf ppf "@[<hov>%a@]: @[<hov>%a@]" pp_phrase group
    Fmt.(list ~sep:(any ",@ ") pp_mailbox)
    mailboxes

let pp_address ppf = function
  | `Group g -> pp_group ppf g
  | `Mailbox m -> pp_mailbox ppf m

let parse_header p ic =
  let decoder = Hd.decoder p in
  let rec go hdr =
    match Hd.decode decoder with
    | `Malformed err -> Error (`Msg err)
    | `Field field -> go (Location.prj field :: hdr)
    | `End _prelude -> Ok (List.rev hdr)
    | `Await ->
    match input_line ic with
    | line ->
        Hd.src decoder (line ^ "\r\n") 0 (String.length line + 2) ;
        go hdr
    | exception End_of_file ->
        Hd.src decoder "" 0 0 ;
        go hdr in
  go []

let pp_value ppf = function
  | `String v -> Fmt.pf ppf "%a" Fmt.(quote string) v
  | `Token v -> Fmt.string ppf v

let pp_parameters ppf = function
  | [] -> ()
  | lst ->
      let pp_binding ppf (k, v) = Fmt.pf ppf "%s=%a" k pp_value v in
      Fmt.pf ppf "; %a" Fmt.(list ~sep:(any ";@,") pp_binding) lst

let pp_content_type ppf { Content_type.ty; subty; parameters } =
  Fmt.pf ppf "%s/%s@[<v>%a@]"
    (Content_type.Type.to_string ty)
    (Content_type.Subtype.to_string subty)
    pp_parameters parameters

let filter hdr fields =
  let rem field_name fields =
    let delete (a, deleted) x =
      if deleted
      then (x :: a, deleted)
      else if Field_name.equal x field_name
      then (a, true)
      else (x :: a, deleted) in
    List.fold_left delete ([], false) fields |> fst |> List.rev in
  let filter (acc, fields) x =
    let (Field.Field (field_name', _, _)) = x in
    if List.exists (Field_name.equal field_name') fields
    then (x :: acc, rem field_name' fields)
    else (acc, fields) in
  List.fold_left filter ([], fields) hdr |> fun (res, _fields) -> List.rev res

let show ~prefix hdr fields =
  let open Field in
  let pp ppf = function
    | Field (field_name, Date, v) ->
        let v, tz_offset_s = R.get_ok (Date.to_ptime v) in
        Fmt.pf ppf "%a: %a" Field_name.pp field_name
          (Ptime.pp_human ~tz_offset_s ())
          v
    | Field (field_name, Addresses, v) ->
        Fmt.pf ppf "%a: @[<v>%a@]" Field_name.pp field_name
          Fmt.(list ~sep:(any ",@,") pp_address)
          v
    | Field (field_name, Mailboxes, v) ->
        Fmt.pf ppf "%a: @[<v>%a@]" Field_name.pp field_name
          Fmt.(list ~sep:(any ",@,") pp_mailbox)
          v
    | Field (field_name, Mailbox, v) ->
        Fmt.pf ppf "%a: %a" Field_name.pp field_name pp_mailbox v
    | Field (field_name, Unstructured, v) ->
        let v =
          let filter a = function #Unstrctrd.elt as elt -> elt :: a | _ -> a in
          List.fold_left filter [] v
          |> List.rev
          |> Unstrctrd.of_list
          |> R.get_ok in
        let v = Unstrctrd.fold_fws v in
        Fmt.pf ppf "%a:%s" Field_name.pp field_name
          (Unstrctrd.to_utf_8_string v)
    | Field (field_name, MessageID, v) ->
        Fmt.pf ppf "%a: %a" Field_name.pp field_name MessageID.pp v
    | Field (field_name, Content, v) ->
        Fmt.pf ppf "%a: %a" Field_name.pp field_name pp_content_type v
    | _ -> () in
  let hdr = match fields with None -> hdr | Some fields -> filter hdr fields in
  List.iter (Fmt.pr "%s%a\n%!" prefix pp) hdr

let show_parameter ~prefix contents key =
  let open Content_type in
  let show_line = function
    | Field.Field (_, Content, content) -> (
        let ps = parameters content in
        match List.assoc key ps with
        | value -> Fmt.pr "%s%a\n%!" prefix pp_value value
        | exception Not_found -> ())
    | _ -> () in
  (* XXX(dinosaure): [assert false]? *)
  List.iter show_line contents

let run fields want_to_decode_rfc2047 prefix parameter input =
  let prefix =
    match (prefix, input) with
    | true, Some fpath -> Fpath.to_string fpath ^ "\t"
    | true, None -> ">\t"
    | _ -> "" in
  decode_rfc2047 := want_to_decode_rfc2047 ;
  let ic, close =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let p =
    match parameter with
    | None -> default
    | Some _ ->
        let fields = Option.fold ~none:[] ~some:identity fields in
        let fields =
          if List.exists (Field_name.equal Field_name.content_type) fields
          then fields
          else Field_name.content_type :: fields in
        let open Field_name in
        List.fold_left
          (fun a x -> Map.add x Field.(Witness Content) a)
          Map.empty fields in
  match
    ( ( parse_header p ic >>| fun res ->
        close ic ;
        res ),
      parameter )
  with
  | Ok hdr, None ->
      show ~prefix hdr fields ;
      `Ok 0
  | Ok hdr, Some parameter ->
      show_parameter ~prefix hdr parameter ;
      `Ok 0
  | Error (`Msg err), _ -> `Error (false, Fmt.str "%s." err)

open Cmdliner

let existing_file =
  let parser = function
    | "-" -> Ok None
    | str ->
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok (Some v)
    | Ok v -> Rresult.R.error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.option ~none:(Fmt.any "-") Fpath.pp)

let field_name = Arg.conv (Field_name.of_string, Field_name.pp)

let input =
  let doc = "The email to analyze." in
  Arg.(value & pos 0 existing_file None & info [] ~doc)

let fields =
  let doc =
    "Only print the values of headers in the colon-separated list $(i,hdrs)."
  in
  Arg.(value & opt (some (list ~sep:':' field_name)) None & info [ "h" ] ~doc)

let parameter =
  let open Content_type in
  Arg.conv (Parameters.key, Fmt.string)

let parameter =
  let doc =
    "Extract a particular $(i,parameter) from the specified $(i,hdrs)." in
  Arg.(value & opt (some parameter) None & info [ "p" ] ~doc)

let decode_rfc2047 =
  let doc = "Decode the $(i,hdrs) according to RFC 2047." in
  Arg.(value & flag & info [ "d" ] ~doc)

let prefix =
  let doc =
    "Prefix output lines with the filename of the message, followed by a tab."
  in
  Arg.(value & flag & info [ "H" ] ~doc)

let cmd =
  let doc = "Print message headers" in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) prints the headers of the specified $(i,msgs).";
    ] in
  ( Term.(ret (const run $ fields $ decode_rfc2047 $ prefix $ parameter $ input)),
    Term.info "hdr" ~doc ~man )

let () = Term.(exit_status @@ eval cmd)
