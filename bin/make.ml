open Mrmime

let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let stream_of_filename_with_crlf filename =
  let ic = open_in filename in
  fun () ->
    match input_line ic with
    | line -> Some (line ^ "\r\n", 0, String.length line + 2)
    | exception End_of_file ->
        close_in ic ;
        None

let stream_of_stdin_with_crlf () =
  match input_line stdin with
  | line -> Some (line ^ "\r\n", 0, String.length line + 2)
  | exception End_of_file -> None

let stream_of_ic ic =
  let tmp = Bytes.create 0x1000 in
  fun () ->
    match input ic tmp 0 (Bytes.length tmp) with
    | 0 -> None
    | len -> Some (Bytes.unsafe_to_string tmp, 0, len)
    | exception End_of_file -> None

let stream_of_stdin = stream_of_ic stdin

let stream_of_filename filename =
  let ic = open_in filename in
  let stream = stream_of_ic ic in
  fun () ->
    match stream () with
    | Some _ as v -> v
    | None ->
        close_in ic ;
        None

let stream_of_stdin_as_lines () =
  match input_line stdin with
  | line -> Some (line, 0, String.length line)
  | exception End_of_file -> None

let stream_of_filename_as_lines filename =
  let ic = open_in filename in
  fun () ->
    match input_line ic with
    | line -> Some (line, 0, String.length line)
    | exception End_of_file ->
        close_in ic ;
        None

let rec stream_to_filename stream filename =
  let oc = open_out filename in
  go stream oc

and go stream oc =
  match stream () with
  | Some (str, off, len) ->
      output_substring oc str off len ;
      go stream oc
  | None -> close_out oc

let rec stream_to_stdout stream =
  match stream () with
  | Some (str, off, len) ->
      output_substring stdout str off len ;
      stream_to_stdout stream
  | None -> ()

let zone_of_tz_offset_s tz_offset_s =
  let hh, mm = (tz_offset_s / 3600, tz_offset_s mod 3600 / 60) in
  match Date.Zone.tz hh mm with
  | Ok tz -> tz
  | Error _ -> Fmt.failwith "Invalid local time-zone: %d second(s)" tz_offset_s

let make _ headers content_encoding mime_type content_parameters from _to cc bcc
    zone with_date body output =
  let hdrs = Header.of_list headers in
  let content_type =
    let ty, subty = mime_type in
    let parameters = Content_type.Parameters.of_list content_parameters in
    Content_type.make ty subty parameters in
  let hdrs =
    hdrs
    |> Header.add_unless_exists Field_name.content_type
         Field.(Content, content_type)
    |> Header.add_unless_exists Field_name.content_encoding
         Field.(Encoding, content_encoding) in
  let hdrs =
    match from with
    | Some (sender :: mailboxes) ->
        hdrs
        |> Header.add_unless_exists Field_name.from
             Field.(Mailboxes, sender :: mailboxes)
        |> Header.add_unless_exists Field_name.sender Field.(Mailbox, sender)
    | Some [] | None -> hdrs in
  let hdrs =
    match _to with
    | Some addresses ->
        Header.add_unless_exists
          Field_name.(v "To")
          Field.(Addresses, addresses)
          hdrs
    | None -> hdrs in
  let hdrs =
    match cc with
    | Some addresses ->
        Header.add Field_name.cc Field.(Addresses, addresses) hdrs
    | None -> hdrs in
  let hdrs =
    match bcc with
    | Some addresses ->
        Header.add Field_name.bcc Field.(Addresses, addresses) hdrs
    | None -> hdrs in
  let hdrs =
    match with_date with
    | `Specific (ptime, None) ->
        let date = Date.of_ptime ~zone ptime in
        Header.add Field_name.date Field.(Date, date) hdrs
    | `Specific (ptime, Some tz_offset_s) ->
        let zone = zone_of_tz_offset_s tz_offset_s in
        let date = Date.of_ptime ~zone ptime in
        Header.add Field_name.date Field.(Date, date) hdrs
    | `Now ->
        let date = Date.of_ptime ~zone (Ptime_clock.now ()) in
        Header.add Field_name.date Field.(Date, date) hdrs
    | `None -> hdrs in
  let body =
    match (body, content_encoding) with
    | "-", (`Bit7 | `Bit8 | `Binary | `Ietf_token _ | `X_token _) ->
        stream_of_stdin_with_crlf
    | filename, (`Bit7 | `Bit8 | `Binary | `Ietf_token _ | `X_token _) ->
        stream_of_filename_with_crlf filename
    | "-", `Base64 -> stream_of_stdin
    | filename, `Base64 -> stream_of_filename filename
    | "-", `Quoted_printable -> stream_of_stdin_as_lines
    | filename, `Quoted_printable -> stream_of_filename_as_lines filename in
  let part = Mt.part ~header:hdrs body in
  let mail = Mt.make Header.empty Mt.simple part in
  let stream = Mt.to_stream mail in
  (match output with
  | Some filename -> stream_to_filename stream (Fpath.to_string filename)
  | None -> stream_to_stdout stream) ;
  `Ok ()

let rec list_hd_map_or ~f ~default = function
  | [] -> default
  | x :: r ->
  match f x with Some x -> x | None -> list_hd_map_or ~f ~default r

let mime_version = Result.get_ok (Unstructured.of_string " 1.0\r\n")

let default =
  let open Field_name in
  Map.empty
  |> Map.add from Field.(Witness Unstructured)
  |> Map.add (v "to") Field.(Witness Unstructured)
  |> Map.add cc Field.(Witness Unstructured)
  |> Map.add bcc Field.(Witness Unstructured)
  |> Map.add sender Field.(Witness Unstructured)
  |> Map.add date Field.(Witness Unstructured)
  |> Map.add subject Field.(Witness Unstructured)
  |> Map.add message_id Field.(Witness Unstructured)
  |> Map.add comments Field.(Witness Unstructured)
  |> Map.add content_type Field.(Witness Content)
  |> Map.add content_encoding Field.(Witness Encoding)

let concat_stream s0 s1 =
  let c = ref s0 in
  let rec go () =
    match !c () with
    | Some x -> Some x
    | None ->
        if !c == s0
        then (
          c := s1 ;
          go ())
        else None in
  go

let stream_of_string str =
  let c = ref false in
  fun () ->
    match !c with
    | true -> None
    | false ->
        c := true ;
        Some (str, 0, String.length str)

let stream_of_in_channel ic () =
  match input_line ic with
  | line when String.length line > 0 && line.[String.length line - 1] = '\r' ->
      Some (line ^ "\n", 0, String.length line + 1)
  | line -> Some (line ^ "\r\n", 0, String.length line + 2)
  | exception End_of_file -> None

let wrap fields g boundary subty input output =
  let ic, ic_close =
    match input with
    | "-" -> (stdin, ignore)
    | filename -> (open_in filename, close_in) in
  let decoder = Hd.decoder default in
  let rec go hdr =
    match Hd.decode decoder with
    | `End prelude -> Ok (prelude, Header.of_list (List.rev hdr))
    | `Field field ->
        let field = Location.prj field in
        go (field :: hdr)
    | `Malformed err -> Error (`Msg err)
    | `Await ->
    match input_line ic with
    | line ->
        let line =
          if String.length line > 0 && line.[String.length line - 1] = '\r'
          then line ^ "\n"
          else line ^ "\r\n" in
        Hd.src decoder line 0 (String.length line) ;
        go hdr
    | exception End_of_file ->
        Hd.src decoder "" 0 0 ;
        go hdr in
  let ( >>= ) = Result.bind in
  go [] >>= fun (prelude, hdr) ->
  let content_type =
    list_hd_map_or
      ~f:(function
        | Field.Field (_, Field.Content, content_type) ->
            Some (content_type : Content_type.t)
        | _ -> None)
      ~default:Content_type.default
      (Header.assoc Field_name.content_type hdr) in
  let content_encoding =
    list_hd_map_or
      ~f:(function
        | Field.Field (_, Field.Encoding, content_encoding) ->
            Some (content_encoding : Content_encoding.t)
        | _ -> None)
      ~default:Content_encoding.default
      (Header.assoc Field_name.content_encoding hdr) in
  let boundary =
    match (g, boundary) with
    | None, None -> Mt.rng ?g:None 8
    | _, Some (`String boundary | `Token boundary) -> boundary
    | Some g, None -> Mt.rng ~g 8 in
  let hdr =
    let content_type =
      Content_type.make `Multipart subty
        Content_type.Parameters.(of_list [ (k "boundary", v boundary) ]) in
    hdr
    |> Header.remove_assoc Field_name.content_type
    |> Header.remove_assoc Field_name.content_encoding
    |> Header.add_unless_exists Field_name.mime_version
         (Field.Unstructured, (mime_version :> Unstructured.t))
    |> Header.add Field_name.content_type (Field.Content, content_type) in
  let stream =
    (* XXX(dinosaure): it's possible to safely concat [prelude]
     * and the rest of [ic] because we only used [input_line] to decode
     * the header, so [prelude] is definitely a line. *)
    concat_stream (stream_of_string prelude) (stream_of_in_channel ic) in
  let part =
    let header =
      Header.of_list
        ([
           Field.Field (Field_name.content_type, Field.Content, content_type);
           Field.Field
             (Field_name.content_encoding, Field.Encoding, content_encoding);
         ]
        @ fields) in
    Mt.part ~encoding:false ~header stream in
  let mail = Mt.multipart ~header:hdr ~rng:Mt.rng ~boundary [ part ] in
  let stream = Mt.to_stream (Mt.make Header.empty Mt.multi mail) in
  (match output with
  | Some filename -> stream_to_filename stream (Fpath.to_string filename)
  | None -> stream_to_stdout stream) ;
  ic_close ic ;
  Ok ()

let wrap _ fields seed boundary subty input output =
  let g =
    match seed with
    | Some seed ->
        Some (Array.init (String.length seed) (fun idx -> Char.code seed.[idx]))
    | None -> None in
  match wrap fields g boundary subty input output with
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)
  | Ok () -> `Ok ()

let ok_if test ~error = match test with true -> Ok () | false -> error ()

let default =
  let open Field_name in
  Map.empty |> Map.add content_type Field.(Witness Content)

let stream_of_queue queue () =
  match Queue.pop queue with
  | line -> Some (line, 0, String.length line)
  | exception Queue.Empty -> None

let stream_of_in_channel_without_close_delimiter ~boundary ic () =
  match input_line ic with
  | line ->
      let close_delimiter = "--" ^ boundary ^ "--" in
      if line = close_delimiter || line = close_delimiter ^ "\r"
      then None
      else if String.length line > 0 && line.[String.length line - 1] = '\r'
      then Some (line ^ "\n", 0, String.length line + 1)
      else Some (line ^ "\r\n", 0, String.length line + 2)
  | exception End_of_file -> None

let put headers content_encoding mime_type content_parameters body input output
    =
  let ( >>= ) = Result.bind in
  let ic, ic_close =
    match input with
    | "-" -> (stdin, ignore)
    | filename -> (open_in filename, close_in) in
  let decoder = Hd.decoder Field_name.Map.empty in
  let queue = Queue.create () in
  let rec go () =
    match Hd.decode decoder with
    | `End _ -> error_msgf "The given email does not have a Content-Type field"
    | `Malformed err -> Error (`Msg err)
    | `Field field -> (
        match Location.prj field with
        | Field.Field (field_name, Field.Content, content_type)
          when Field_name.equal field_name Field_name.content_type ->
            Ok (content_type : Content_type.t)
        | _ -> go ())
    | `Await ->
    match input_line ic with
    | line ->
        let line =
          if String.length line > 0 && line.[String.length line - 1] = '\r'
          then line ^ "\n"
          else line ^ "\r\n" in
        Queue.push line queue ;
        Hd.src decoder line 0 (String.length line) ;
        go ()
    | exception End_of_file ->
        Hd.src decoder "" 0 0 ;
        go () in
  go () >>= fun content_type ->
  ok_if (Content_type.is_multipart content_type) ~error:(fun () ->
      error_msgf "The given email does not contain multiple parts")
  >>= fun () ->
  let parameters =
    Content_type.Parameters.of_list content_type.Content_type.parameters in
  let none = msgf "Content-Type does not contain a boundary parameter" in
  Content_type.Parameters.(find (k "boundary") parameters)
  |> Option.to_result ~none
  >>= fun boundary ->
  let boundary = match boundary with `Token v -> v | `String v -> v in
  let hdrs = Header.of_list headers in
  let content_type =
    let ty, subty = mime_type in
    let parameters = Content_type.Parameters.of_list content_parameters in
    Content_type.make ty subty parameters in
  let hdrs =
    hdrs
    |> Header.add_unless_exists Field_name.content_type
         Field.(Content, content_type)
    |> Header.add_unless_exists Field_name.content_encoding
         Field.(Encoding, content_encoding) in
  let body =
    match content_encoding with
    | `Base64 -> stream_of_filename body
    | `Quoted_printable -> stream_of_filename_as_lines body
    | `Bit7 | `Bit8 | `Binary | `Ietf_token _ | `X_token _ ->
        stream_of_filename_with_crlf body in
  let part = Mt.part ~header:hdrs body in
  let mail = Mt.make Header.empty Mt.simple part in
  let stream0 =
    concat_stream (stream_of_queue queue)
      (stream_of_in_channel_without_close_delimiter ~boundary ic) in
  let stream1 =
    concat_stream
      (stream_of_string ("--" ^ boundary ^ "\r\n"))
      (Mt.to_stream mail) in
  let stream2 = concat_stream stream0 stream1 in
  let stream =
    concat_stream stream2 (stream_of_string ("\r\n--" ^ boundary ^ "--\r\n"))
  in
  (match output with
  | Some filename -> stream_to_filename stream (Fpath.to_string filename)
  | None -> stream_to_stdout stream) ;
  ic_close ic ;
  Ok ()

let add_field _ headers content_encoding mime_type content_parameters from _to
    cc bcc zone with_date input output =
  let hdrs = Header.of_list headers in
  let content_type =
    match (mime_type, content_parameters) with
    | Some (ty, subty), parameters ->
        let parameters = Content_type.Parameters.of_list parameters in
        Ok (Some (Content_type.make ty subty parameters))
    | None, _ :: _ ->
        error_msgf "Impossible to add a Content-Type field without MIME type"
    | None, [] -> Ok None in
  let hdrs =
    match content_encoding with
    | Some encoding ->
        Header.add_unless_exists Field_name.content_encoding
          Field.(Encoding, encoding)
          hdrs
    | None -> hdrs in
  let hdrs =
    match from with
    | Some (sender :: mailboxes) ->
        hdrs
        |> Header.add_unless_exists Field_name.from
             Field.(Mailboxes, sender :: mailboxes)
        |> Header.add_unless_exists Field_name.sender Field.(Mailbox, sender)
    | Some [] | None -> hdrs in
  let hdrs =
    match _to with
    | Some addresses ->
        Header.add_unless_exists
          Field_name.(v "To")
          Field.(Addresses, addresses)
          hdrs
    | None -> hdrs in
  let hdrs =
    match cc with
    | Some addresses ->
        Header.add Field_name.cc Field.(Addresses, addresses) hdrs
    | None -> hdrs in
  let hdrs =
    match bcc with
    | Some addresses ->
        Header.add Field_name.bcc Field.(Addresses, addresses) hdrs
    | None -> hdrs in
  let hdrs =
    match with_date with
    | `Specific (ptime, None) ->
        let date = Date.of_ptime ~zone ptime in
        Header.add Field_name.date Field.(Date, date) hdrs
    | `Specific (ptime, Some tz_offset_s) ->
        let zone = zone_of_tz_offset_s tz_offset_s in
        let date = Date.of_ptime ~zone ptime in
        Header.add Field_name.date Field.(Date, date) hdrs
    | `Now ->
        let date = Date.of_ptime ~zone (Ptime_clock.now ()) in
        Header.add Field_name.date Field.(Date, date) hdrs
    | `None -> hdrs in
  match content_type with
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)
  | Ok content_type ->
      let hdrs =
        match content_type with
        | Some content_type ->
            Header.add Field_name.content_type
              Field.(Content, content_type)
              hdrs
        | None -> hdrs in
      let w ppf v =
        let noop = ((fun ppf () -> ppf), ()) in
        (Prettym.list ~sep:noop Field.Encoder.field) ppf (Header.to_list v)
      in
      let stream = Prettym.to_stream w hdrs in
      let stream () =
        match stream () with
        | Some str -> Some (str, 0, String.length str)
        | None -> None in
      let stream =
        match input with
        | "-" -> concat_stream stream stream_of_stdin
        | filename -> concat_stream stream (stream_of_filename filename) in
      (match output with
      | Some filename -> stream_to_filename stream (Fpath.to_string filename)
      | None -> stream_to_stdout stream) ;
      `Ok ()

let put _ headers content_encoding mime_type content_parameters body input
    output =
  match
    put headers content_encoding mime_type content_parameters body input output
  with
  | Ok () -> `Ok ()
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

open Cmdliner
open Blaze_cli

let field =
  let parser str =
    match String.split_on_char ':' str with
    | [ field_name; value ] -> (
        match
          ( Field_name.of_string field_name,
            Unstructured.of_string (value ^ "\r\n") )
        with
        | Ok field_name, Ok unstrctrd ->
            Ok
              (Field.make field_name Field.Unstructured
                 (unstrctrd :> Unstructured.elt list))
        | Error _, _ -> error_msgf "Invalid field-name: %S" field_name
        | _, Error _ -> error_msgf "Invalid unstructred value: %S" value)
    | _ -> error_msgf "Invalid field: %S" str in
  let pp ppf (Field.Field (field_name, w, v)) =
    match w with
    | Field.Unstructured ->
        Fmt.pf ppf "%a:%a" Field_name.pp field_name Unstructured.pp v
    | _ -> assert false in
  Arg.conv (parser, pp)

let content_encoding = Arg.conv (Content_encoding.of_string, Content_encoding.pp)

let content_type =
  let ( >>= ) = Result.bind in
  let parser str =
    match String.split_on_char '/' str with
    | [ ty; subty ] ->
        Content_type.Type.of_string ty >>= fun ty ->
        Content_type.Subtype.iana ty subty >>= fun subty -> Ok (ty, subty)
    | _ -> error_msgf "Invalid content-type: %S" str in
  let pp ppf (ty, subty) =
    Fmt.pf ppf "%a/%a" Content_type.Type.pp ty Content_type.Subtype.pp subty
  in
  Arg.conv (parser, pp)

let mailboxes =
  let parser str =
    let lst = String.split_on_char ',' str in
    let f acc str =
      match acc with
      | Error _ as err -> err
      | Ok acc ->
      match Emile.of_string str with
      | Ok mailbox -> Ok (mailbox :: acc)
      | Error _ -> error_msgf "Invalid mailbox: %S" str in
    List.fold_left f (Ok []) lst in
  let pp ppf lst = Fmt.pf ppf "%a" Fmt.(Dump.list Emile.pp_mailbox) lst in
  Arg.conv (parser, pp)

let mailbox =
  let parser str =
    match Emile.of_string str with
    | Ok _ as v -> v
    | Error _ -> error_msgf "Invalid mailbox: %S" str in
  Arg.conv (parser, Emile.pp_mailbox)

let address =
  let parser str =
    match Emile.set_of_string str with
    | Ok _ as v -> v
    | Error _ -> error_msgf "Invalid address: %S" str in
  Arg.conv (parser, Emile.pp)

let filename =
  let ( >>| ) x fn = Result.map fn x in
  let parser = function
    | "-" -> Ok None
    | str -> Fpath.of_string str >>| fun v -> Some v in
  let pp ppf = function
    | Some v -> Fpath.pp ppf v
    | None -> Fmt.string ppf "-" in
  Arg.conv (parser, pp)

let date =
  let parser str =
    match String.lowercase_ascii str with
    | "none" -> Ok `None
    | "now" -> Ok `Now
    | _ ->
    match Ptime.of_rfc3339 str with
    | Ok (ptime, tz, _) -> Ok (`Specific (ptime, tz))
    | Error (`RFC3339 (_, err)) ->
        let msg = Fmt.str "%a" Ptime.pp_rfc3339_error err in
        Error (`Msg msg) in
  let pp ppf = function
    | `None -> Fmt.string ppf "none"
    | `Specific (ptime, tz_offset_s) ->
        Fmt.pf ppf "%a" (Ptime.pp_rfc3339 ?tz_offset_s ()) ptime
    | `Now -> Fmt.string ppf "now" in
  Arg.conv (parser, pp)

let content_parameter =
  let ( >>= ) = Result.bind in
  let parser str =
    match String.split_on_char '=' str with
    | [ k; v ] ->
        Content_type.Parameters.key k >>= fun key ->
        Content_type.Parameters.value v >>= fun value -> Ok (key, value)
    | _ -> error_msgf "Invalid parameter: %S" str in
  let pp ppf (k, v) =
    Fmt.pf ppf "%a=%a" Content_type.Parameters.pp_key k
      Content_type.Parameters.pp_value v in
  Arg.conv (parser, pp)

let setup_zone = function
  | Some zone -> zone
  | None ->
      let tm = Unix.time () in
      let gmt = Unix.gmtime tm and local = Unix.localtime tm in
      let gmt, _ = Unix.mktime gmt and local, _ = Unix.mktime local in
      let tz_offset_s = int_of_float (local -. gmt) in
      zone_of_tz_offset_s tz_offset_s

let zone = Arg.conv (Date.Zone.of_string, Date.Zone.pp)

let zone =
  let env = Cmd.Env.info "BLAZE_ZONE" in
  let doc = "Time-zone." in
  Arg.(value & opt (some zone) None & info [ "zone" ] ~env ~doc)

let setup_zone = Term.(const setup_zone $ zone)

let headers =
  let doc = "Field name and its value." in
  Arg.(value & opt_all field [] & info [ "f"; "field" ] ~doc)

let content_parameters =
  let doc = "Parameter for the Content-Type value." in
  Arg.(
    value
    & opt_all content_parameter
        Content_type.Parameters.[ (key_exn "charset", value_exn "utf-8") ]
    & info [ "p"; "parameter" ] ~doc)

let from =
  let doc = "The sender of the email." in
  Arg.(value & opt (some mailboxes) None & info [ "from" ] ~doc)

let _to =
  let doc = "Recipients of the email." in
  Arg.(value & opt (some (list ~sep:',' address)) None & info [ "to" ] ~doc)

let cc =
  let doc = "Secondary recipients of the email." in
  Arg.(value & opt (some (list ~sep:',' address)) None & info [ "cc" ] ~doc)

let bcc =
  let doc = "Tertiary recipients of the email." in
  Arg.(value & opt (some (list ~sep:',' address)) None & info [ "bcc" ] ~doc)

let date =
  let doc = "Date of the email." in
  Arg.(value & opt date `Now & info [ "date" ] ~doc)

let body =
  let doc = "Body of the email. Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos ~rev:true 0 Blaze_cli.file "-" & info [] ~doc)

let output =
  let doc = "The filename where you want to save the email." in
  Arg.(value & opt filename None & info [ "o"; "output" ] ~doc)

let make_info, make_term =
  let content_encoding =
    let doc = "Encoding of the body." in
    Arg.(value & opt content_encoding `Bit7 & info [ "encoding" ] ~doc) in
  let mime_type =
    let doc = "MIME type of the body." in
    Arg.(
      value
      & opt content_type (`Text, Content_type.Subtype.v `Text "plain")
      & info [ "type" ] ~doc) in
  let doc = "Craft an email with an header." in
  let man =
    [
      `S Manpage.s_description;
      `P "Craft an email with an header given by the user.";
    ] in
  let term =
    let open Term in
    const make
    $ setup_logs
    $ headers
    $ content_encoding
    $ mime_type
    $ content_parameters
    $ from
    $ _to
    $ cc
    $ bcc
    $ setup_zone
    $ date
    $ body
    $ output
    |> ret in
  (Cmd.info "make" ~doc ~man, term)

let input =
  let doc = "The email to be modified. Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos ~rev:true 0 Blaze_cli.file "-" & info [] ~doc)

let add_field =
  let content_encoding =
    let doc = "Encoding of the body." in
    Arg.(value & opt (some content_encoding) None & info [ "encoding" ] ~doc)
  in
  let mime_type =
    let doc = "MIME type of the body." in
    Arg.(value & opt (some content_type) None & info [ "type" ] ~doc) in
  let content_parameters =
    let doc = "Parameter for the Content-Type value." in
    Arg.(value & opt_all content_parameter [] & info [ "p"; "parameter" ] ~doc)
  in
  let doc = "Prepend the given email with some fields." in
  let man =
    [ `S Manpage.s_description; `P "Prepend the given email with some fields." ]
  in
  let term =
    let open Term in
    const add_field
    $ setup_logs
    $ headers
    $ content_encoding
    $ mime_type
    $ content_parameters
    $ from
    $ _to
    $ cc
    $ bcc
    $ setup_zone
    $ date
    $ input
    $ output
    |> ret in
  Cmd.v (Cmd.info "add-field" ~doc ~man) term

let input =
  let doc =
    "The email to wrap into a multipart one. Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos ~rev:true 0 Blaze_cli.file "-" & info [] ~doc)

let seed =
  let doc = "Seed used by the random number generator." in
  let base64 =
    Arg.conv
      ((fun str -> Base64.decode str), Fmt.using Base64.encode_string Fmt.string)
  in
  Arg.(value & opt (some base64) None & info [ "s"; "seed" ] ~doc)

let boundary =
  let doc = "Boundary used to delimit parts." in
  let content_type_value =
    Arg.conv (Content_type.Parameters.value, Content_type.Parameters.pp_value)
  in
  Arg.(value & opt (some content_type_value) None & info [ "boundary" ] ~doc)

let subty =
  let mixed = Content_type.Subtype.v `Multipart "mixed" in
  let flags =
    [
      (mixed, Arg.info [ "mixed" ]);
      (Content_type.Subtype.v `Multipart "form-data", Arg.info [ "form-data" ]);
      (Content_type.Subtype.v `Multipart "parallel", Arg.info [ "parallel" ]);
      (Content_type.Subtype.v `Multipart "related", Arg.info [ "related" ]);
      (Content_type.Subtype.v `Multipart "report", Arg.info [ "report" ]);
      (Content_type.Subtype.v `Multipart "signed", Arg.info [ "signed" ]);
    ] in
  Arg.(value & vflag mixed flags)

let wrap =
  let doc = "Wrap the given email into a multipart one." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Wrap the given email into a multipart one. The header of the given \
         email does not change, only $(i,Content-Type) and \
         $(i,Content-Transfer-Encoding) are updated. The new part will keep \
         the same old $(i,Content-Type) and $(i,Content-Transfer-Encoding) \
         value.";
    ] in
  let term =
    let open Term in
    const wrap
    $ setup_logs
    $ headers
    $ seed
    $ boundary
    $ subty
    $ input
    $ output
    |> ret in
  Cmd.v (Cmd.info "wrap" ~doc ~man) term

let input =
  let doc =
    "The email to wrap into a multipart one. Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos 1 Blaze_cli.file "-" & info [] ~doc)

let body =
  let doc = "Body of the email." in
  Arg.(value & pos 0 Blaze_cli.file "-" & info [] ~doc)

let put =
  let content_encoding =
    let doc = "Encoding of the body." in
    Arg.(value & opt content_encoding `Bit7 & info [ "encoding" ] ~doc) in
  let mime_type =
    let doc = "MIME type of the body." in
    Arg.(
      value
      & opt content_type (`Text, Content_type.Subtype.v `Text "plain")
      & info [ "type" ] ~doc) in
  let doc = "Put a new part into the given multipart email." in
  let man =
    [
      `S Manpage.s_description;
      `P "Put a new part into the given multipart email.";
    ] in
  let term =
    let open Term in
    const put
    $ setup_logs
    $ headers
    $ content_encoding
    $ mime_type
    $ content_parameters
    $ body
    $ input
    $ output
    |> ret in
  Cmd.v (Cmd.info "put" ~doc ~man) term

let cmd = Cmd.group ~default:make_term make_info [ add_field; wrap; put ]
