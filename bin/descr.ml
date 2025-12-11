open Mrmime

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let default =
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

type t = Single of { mime : string option } | Choose of kind * t list
and kind = [ `Mixed | `Alternative | `Parallel | `Message | `Other of string ]

let pp_kind ppf = function
  | `Mixed -> Fmt.string ppf "mixed"
  | `Alternative -> Fmt.string ppf "alternative"
  | `Parallel -> Fmt.string ppf "parallel"
  | `Message -> Fmt.string ppf "message"
  | `Other v -> Fmt.string ppf v

let top =
  Lazy.from_fun @@ fun () ->
  if Fmt.utf_8 Fmt.stdout then ("┌── ", "│   ") else (".-- ", "|   ")

let between =
  Lazy.from_fun @@ fun () ->
  if Fmt.utf_8 Fmt.stdout then ("├── ", "│   ") else ("|-- ", "|   ")

let last =
  Lazy.from_fun @@ fun () ->
  if Fmt.utf_8 Fmt.stdout then ("└── ", "    ") else ("`-- ", "    ")

let rec pp ?(prefix = "") ?(is_last = false) ppf tree =
  let branch, next_prefix = Lazy.force (if is_last then last else between) in
  match tree with
  | Single { mime } ->
      let mime = Option.value ~default:"unknown" mime in
      Fmt.pf ppf "%s%s%s@." prefix branch mime
  | Choose (kind, children) ->
      Fmt.pf ppf "%s%s%a@." prefix branch pp_kind kind ;
      let prefix = prefix ^ next_prefix in
      let rec go = function
        | [] -> ()
        | [ x ] -> pp ~prefix ~is_last:true ppf x
        | x :: r ->
            pp ~prefix ~is_last:false ppf x ;
            go r in
      go children

let pp ppf = function
  | Single _ as v -> pp ppf v
  | Choose (kind, children) ->
      let branch, next_prefix = Lazy.force top in
      Fmt.pf ppf "%s%a@." branch pp_kind kind ;
      let prefix = next_prefix in
      let rec go = function
        | [] -> ()
        | [ x ] -> pp ~prefix ~is_last:true ppf x
        | x :: r ->
            pp ~prefix ~is_last:false ppf x ;
            go r in
      go children

let blit src src_off dst dst_off len =
  Bstr.blit_from_string src ~src_off dst ~dst_off ~len

let parser ic =
  let emitters _headers = (Fun.const (), ()) in
  let parser = Mrmime.Mail.stream ~transfer_encoding:false ~g:default emitters in
  let rec loop ic ke = function
    | Angstrom.Unbuffered.Done (_, v) -> Ok v
    | Fail (_, stack, msg) ->
        Logs.err (fun m ->
            m "Invalid email (%a): %S" Fmt.(Dump.list string) stack msg) ;
        error_msgf "Invalid email"
    | Partial { committed; continue } -> begin
        Ke.Rke.N.shift_exn ke committed ;
        if committed = 0 then Ke.Rke.compress ke ;
        match input_line ic with
        | "" ->
            Ke.Rke.push ke '\n' ;
            let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
            let off = 0 and len = Bstr.length slice in
            let state = continue slice ~off ~len Incomplete in
            loop ic ke state
        | line when line.[String.length line - 1] = '\r' ->
            Ke.Rke.N.push ke ~blit ~length:String.length ~off:0
              ~len:(String.length line) line ;
            Ke.Rke.push ke '\n' ;
            let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
            let off = 0 and len = Bstr.length slice in
            let state = continue slice ~off ~len Incomplete in
            loop ic ke state
        | line ->
            Ke.Rke.N.push ke ~blit ~length:String.length ~off:0
              ~len:(String.length line) line ;
            Ke.Rke.push ke '\r' ;
            Ke.Rke.push ke '\n' ;
            let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
            let off = 0 and len = Bstr.length slice in
            let state = continue slice ~off ~len Incomplete in
            loop ic ke state
        | exception End_of_file ->
            let buf =
              match Ke.Rke.length ke with
              | 0 -> Bstr.empty
              | _ ->
                  Ke.Rke.compress ke ;
                  List.hd (Ke.Rke.N.peek ke) in
            let off = 0 and len = Bstr.length buf in
            let state = continue buf ~off ~len Complete in
            loop ic ke state
      end in
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  loop ic ke (Angstrom.Unbuffered.parse parser)

let mime_from_headers hdrs =
  let open Mrmime in
  let content_type = Field_name.content_type in
  match Header.assoc content_type hdrs with
  | Field.Field (_, Content, v) :: _ ->
      let a = Content_type.Type.to_string v.Content_type.ty in
      let b = Content_type.Subtype.to_string v.Content_type.subty in
      Some (Fmt.str "%s/%s" a b)
  | _ -> None

let to_kind v =
  let v = Content_type.Subtype.to_string v in
  let v = String.lowercase_ascii v in
  match v with
  | "mixed" -> `Mixed
  | "alternative" -> `Alternative
  | "parallel" -> `Parallel
  | v -> `Other v

let kind_of_headers hdrs =
  let open Mrmime in
  let content_type = Field_name.content_type in
  match Header.assoc content_type hdrs with
  | Field.Field (_, Content, v) :: _ -> to_kind v.Content_type.subty
  | _ -> assert false

let rec to_semantic ~headers = function
  | Mrmime.Mail.Leaf _ ->
      let mime = mime_from_headers headers in
      Single { mime }
  | Multipart lst ->
      let kind = kind_of_headers headers in
      let fn acc (headers, contents) =
        match contents with
        | None -> acc
        | Some contents -> to_semantic ~headers contents :: acc in
      let lst = List.fold_left fn [] lst in
      let lst = List.rev lst in
      Choose (kind, lst)
  | Message (headers, t) ->
      let t = to_semantic ~headers t in
      Choose (`Message, [ t ])

let to_semantic (headers, t) = to_semantic ~headers t
let ( let@ ) finally fn = Fun.protect ~finally fn

let run _ input =
  let ic, close =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let@ () = fun () -> close ic in
  match parser ic with
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)
  | Ok v ->
      Fmt.pr "%a%!" pp (to_semantic v) ;
      `Ok ()

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

let field_name = Arg.conv (Field_name.of_string, Field_name.pp)

let input =
  let doc = "The email to analyze." in
  Arg.(value & pos 0 existing_file None & info [] ~doc)

let cmd =
  let doc = "Describe the structure of the given email." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) describes the structure of the given email like a tree.";
    ] in
  let info = Cmd.info "descr" ~doc ~man in
  let term =
    let open Term in
    ret (const run $ setup_logs $ input) in
  Cmd.v info term
