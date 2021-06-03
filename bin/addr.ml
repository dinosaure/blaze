open Mrmime

let ( <.> ) f g x = f (g x)

let default =
  let open Field_name in
  Map.empty
  |> Map.add from Field.(Witness Mailboxes)
  |> Map.add (v "to") Field.(Witness Addresses)
  |> Map.add cc Field.(Witness Addresses)
  |> Map.add bcc Field.(Witness Addresses)
  |> Map.add sender Field.(Witness Mailbox)

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
  Fmt.(list ~sep:(always "@ ") pp_elem) ppf phrase

let pp_mailbox ppf = function
  | { Emile.name = None; _ } as v -> Emile.pp_mailbox ppf v
  | { name = Some name; domain = _, []; _ } as v ->
      Fmt.pf ppf "@[<hov>%a@] <%a>" pp_phrase name Emile.pp_mailbox
        { v with Emile.name = None }
  | { name = Some name; _ } as v ->
      (* XXX(dinosaure): with multiple domains, we know that [emile] surrounds with "<" and ">". *)
      Fmt.pf ppf "@[<hov>%a@] %a" pp_phrase name Emile.pp_mailbox
        { v with Emile.name = None }

let parse_header p ic =
  let open Rresult in
  let decoder = Hd.decoder p in
  let rec go (addresses : Emile.mailbox list) =
    match Hd.decode decoder with
    | `Malformed err -> Error (`Msg err)
    | `Field field -> (
        match Location.prj field with
        | Field (_field_name, Mailboxes, vs) -> go (vs @ addresses)
        | Field (_field_name, Mailbox, v) -> go (v :: addresses)
        | Field (_field_name, Addresses, vs) ->
            let vs =
              let f = function
                | `Group { Emile.mailboxes; _ } -> mailboxes
                | `Mailbox m -> [ m ] in
              List.concat (List.map f vs) in
            go (vs @ addresses)
        | _ -> go addresses)
    | `End _prelude -> Ok (List.rev addresses)
    | `Await ->
    match input_line ic with
    | line ->
        Hd.src decoder (line ^ "\r\n") 0 (String.length line + 2) ;
        go addresses
    | exception End_of_file ->
        Hd.src decoder "" 0 0 ;
        go addresses in
  go []

let run want_to_decode_rfc2047 fields input =
  decode_rfc2047 := want_to_decode_rfc2047 ;
  let ic, close =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let open Rresult in
  let p =
    List.fold_left
      (fun p v -> Field_name.Map.add v Field.(Witness Addresses) p)
      default fields in
  match
    parse_header p ic >>| fun res ->
    close ic ;
    res
  with
  | Ok addresses ->
      List.iter (print_endline <.> Fmt.str "%a" pp_mailbox) addresses ;
      `Ok 0
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

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
  let doc = "Extra-fields which contains email addresses." in
  Arg.(value & opt (list ~sep:':' field_name) [] & info [ "f"; "fields" ] ~doc)

let decode_rfc2047 =
  let doc = "Decode the $(i,hdrs) according to RFC 2047." in
  Arg.(value & flag & info [ "d" ] ~doc)

let cmd =
  let doc = "Extract addresses from an email." in
  let man =
    [
      `S Manpage.s_description; `P "$(tname) extracts addresses from an email.";
    ] in
  ( Term.(ret (const run $ decode_rfc2047 $ fields $ input)),
    Term.info "addr" ~doc ~man )

let () = Term.(exit_status @@ eval cmd)
