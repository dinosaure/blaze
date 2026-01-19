let ( let@ ) finally fn = Fun.protect ~finally fn

let default =
  let open Mrmime in
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
    | `Encoded (charset, Emile.Quoted_printable (Ok v)) when !decode_rfc2047 ->
        let v' = Rosetta.to_utf_8_string ~charset v in
        if Option.is_none v'
        then
          Logs.warn (fun m ->
              m "Impossible to normalize %S (charset: %s) to UTF-8" v charset) ;
        let v' = Option.value ~default:v v' in
        Fmt.string ppf v'
    | `Encoded (charset, Emile.Base64 (Ok v)) when !decode_rfc2047 ->
        let v' = Rosetta.to_utf_8_string ~charset v in
        if Option.is_none v'
        then
          Logs.warn (fun m ->
              m "Impossible to normalize %S (charset: %s) to UTF-8" v charset) ;
        let v' = Option.value ~default:v v' in
        Fmt.string ppf v'
    | `Encoded (charset, v) -> pp_encoded ~charset ppf v in
  Fmt.(list ~sep:(any "@ ") pp_elem) ppf phrase

let pp_mailbox ppf = function
  | { Emile.name = None; _ } as v -> Emile.pp_mailbox ppf v
  | { name = Some name; domain = _, []; _ } as v ->
      Fmt.pf ppf "@[<1>@[<hov>%a@]@ <%a>@]" pp_phrase name Emile.pp_mailbox
        { v with Emile.name = None }
  | { name = Some name; _ } as v ->
      (* XXX(dinosaure): with multiple domains, we know that [emile] surrounds with "<" and ">". *)
      Fmt.pf ppf "@[<1>@[<hov>%a@]@ %a@]" pp_phrase name Emile.pp_mailbox
        { v with Emile.name = None }

let pp_mailbox_without_name ppf = function
  | { Emile.local; domain = domain, _; _ } ->
      Fmt.pf ppf "@[<1>%a@]" Emile.pp_mailbox
        { Emile.local; domain = (domain, []); name = None }

let pp_mailbox ~without_name =
  match without_name with
  | true -> pp_mailbox_without_name
  | false -> pp_mailbox

let parse_header newline p ic =
  let ( let* ) = Result.bind in
  let lines ic =
    match newline with
    | `LF ->
        let init = Fun.const ic
        and pull ic =
          match input_line ic with
          | line -> Some (line ^ "\r\n", ic)
          | exception End_of_file -> None
        and stop = Fun.const () in
        Flux.Source { init; pull; stop }
    | `CRLF ->
        let init = Fun.const ic
        and pull ic =
          match input_line ic with
          | line -> Some (line ^ "\n", ic)
          | exception End_of_file -> None
        and stop = Fun.const () in
        Flux.Source { init; pull; stop } in
  let src = lines ic in
  let into = Header.parser p in
  let* _prelude, hdrs = Flux.Stream.from src |> Flux.Stream.into into in
  let fn : _ -> Emile.mailbox list = function
    | Mrmime.Field.Field (_, Mailboxes, vs) -> vs
    | Field (_, Mailbox, v) -> [ v ]
    | Field (_, Addresses, vs) ->
        let fn = function
          | `Group { Emile.mailboxes; _ } -> mailboxes
          | `Mailbox m -> [ m ] in
        List.concat_map fn vs
    | _ -> [] in
  let addresses = List.concat_map fn (Mrmime.Header.to_list hdrs) in
  Ok addresses

let run want_to_decode_rfc2047 newline without_name margin fields input =
  Option.iter Format.set_margin margin ;
  decode_rfc2047 := want_to_decode_rfc2047 ;
  let open Mrmime in
  let ic, ic_close =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let@ () = fun () -> ic_close ic in
  let p =
    List.fold_left
      (fun p v -> Field_name.Map.add v Field.(Witness Addresses) p)
      default fields in
  match parse_header newline p ic with
  | Ok addresses ->
      let fn =
        Fun.compose print_endline (Fmt.str "%a" (pp_mailbox ~without_name))
      in
      List.iter fn addresses ;
      `Ok ()
  | Error _ -> `Error (false, "Invalid email.")

open Cmdliner
open Blaze_cli

let existing_file =
  let parser = function
    | "-" -> Ok None
    | str ->
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok (Some v)
    | Ok v -> error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.option ~none:(Fmt.any "-") Fpath.pp)

let field_name =
  let open Mrmime in
  Arg.conv (Field_name.of_string, Field_name.pp)

let input =
  let doc = "The email to analyze." in
  Arg.(value & pos 0 existing_file None & info [] ~doc ~docv:"EMAIL")

let fields =
  let doc = "Extra-fields which contains email addresses." in
  let open Arg in
  value
  & opt (list ~sep:':' field_name) []
  & info [ "f"; "fields" ] ~doc ~docv:"FIELD"

let decode_rfc2047 =
  let doc =
    "Decode $(i,quoted-printable)/$(i,base64) values according to RFC 2047 and \
     normalize them to UTF-8 (best-effort)." in
  Arg.(value & flag & info [ "d" ] ~doc)

let without_name =
  let doc = "Show email addresses without their names." in
  Arg.(value & flag & info [ "without-name" ] ~doc)

let margin =
  let doc = "Set the margin which is our limit to print email addresses." in
  let number =
    let parser str =
      match int_of_string_opt str with
      | Some n when n >= 1 -> Ok n
      | Some _ -> error_msgf "The margin must be greater or equal to 1"
      | None -> error_msgf "Invalid margin" in
    Arg.conv (parser, Fmt.int) in
  let open Arg in
  value & opt (some number) None & info [ "m"; "margin" ] ~doc ~docv:"MARGIN"

let cmd =
  let doc = "Extract addresses from an email." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) extracts email addresses from an email.";
      `P
        "This can be useful for automating who can be replied to from an \
         email. The program may seem simple, but it allows you to display the \
         email addresses collected. The program attempts to normalize the \
         values (especially names) to UTF-8 and tries to respect a margin in \
         the display.";
      `P
        "If an email address is larger than the margin, the program behaves \
         like $(i,RFC822) and outputs a newline and a space as the \
         continuation of the email address.";
      `P
        "It is possible to keep the $(i,RFC2047) representation of values (pre \
         UTF-8) allowing names to be encoded securely between machines (the \
         $(i,RFC2047) representation ensures that it can be transmitted via \
         7-bit encoding).";
    ] in
  let info = Cmd.info "addr" ~doc ~man in
  let term =
    let open Term in
    const run
    $ decode_rfc2047
    $ newline ()
    $ without_name
    $ margin
    $ fields
    $ input
    |> ret in
  Cmd.v info term
