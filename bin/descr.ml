open Rresult
open Mrmime

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

let emitter_of_queue q = function Some str -> Queue.push str q | None -> ()

let blit src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let up_to_right =
  Lazy.from_fun @@ fun () ->
  if Fmt.utf_8 Fmt.stdout then "\xe2\x94\x97" else "`-"

let up_and_right =
  Lazy.from_fun @@ fun () ->
  if Fmt.utf_8 Fmt.stdout then "\xe2\x94\xa3" else "|-"

let up_to_down =
  Lazy.from_fun @@ fun () ->
  if Fmt.utf_8 Fmt.stdout then "\xe2\x94\x83" else "| "

let parser ic =
  let uid = ref (-1) in
  let tbl = Hashtbl.create 0x10 in
  let emitters _header =
    incr uid ;
    let v = !uid in
    let contents = Queue.create () in
    Hashtbl.add tbl v contents ;
    (emitter_of_queue contents, v) in
  let parser = Mrmime.Mail.stream ~g:default emitters in
  let rec loop ic ke = function
    | Angstrom.Unbuffered.Done (_, (header, mail)) -> R.ok (header, mail, tbl)
    | Fail _ -> R.error_msgf "Invalid incoming email"
    | Partial { committed; continue } -> (
        Ke.Rke.N.shift_exn ke committed ;
        if committed = 0 then Ke.Rke.compress ke ;
        match input_line ic with
        | "" ->
            Ke.Rke.push ke '\n' ;
            let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
            loop ic ke
              (continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete)
        | line when line.[String.length line - 1] = '\r' ->
            Ke.Rke.N.push ke ~blit ~length:String.length ~off:0
              ~len:(String.length line) line ;
            Ke.Rke.push ke '\n' ;
            let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
            loop ic ke
              (continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete)
        | line ->
            Ke.Rke.N.push ke ~blit ~length:String.length ~off:0
              ~len:(String.length line) line ;
            Ke.Rke.push ke '\r' ;
            Ke.Rke.push ke '\n' ;
            let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
            loop ic ke
              (continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete)
        | exception End_of_file ->
            let buf =
              match Ke.Rke.length ke with
              | 0 -> Bigstringaf.empty
              | _ ->
                  Ke.Rke.compress ke ;
                  List.hd (Ke.Rke.N.peek ke) in
            loop ic ke
              (continue buf ~off:0 ~len:(Bigstringaf.length buf) Complete))
  in
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  loop ic ke (Angstrom.Unbuffered.parse parser)

let pp_unstructured ppf unstructured =
  let lst =
    List.fold_left
      (fun a -> function #Unstrctrd.elt as x -> x :: a | _ -> a)
      [] unstructured in
  let lst = List.rev lst in
  let v = R.get_ok (Unstrctrd.of_list lst) in
  Fmt.pf ppf "@[<hov>%s@]" (Unstrctrd.to_utf_8_string v)

let show_headers ~fields ~level hdrs =
  let up_to_right = Lazy.force up_to_right in
  let up_to_down = Lazy.force up_to_down in

  let lst = Header.to_list hdrs in
  let lst, _ =
    match fields with
    | `All -> (lst, [])
    | `Some vs ->
        let predicate (Field.Field (field_name, _, _)) =
          List.exists (Field_name.equal field_name) vs in
        List.partition predicate lst in
  let max = List.length lst in

  let show_field ~prefix pp field_name v =
    Fmt.pr "%s%a:%a\n%!" prefix Field_name.pp field_name pp v in

  let f idx field =
    let prefix =
      if idx = max - 1 && level > 0
      then Fmt.str "%s%s " (String.make (level - 1) ' ') up_to_right
      else if idx > 0 && level > 0
      then Fmt.str "%s%s " (String.make (level - 1) ' ') up_to_down
      else "" in
    match field with
    | Field.Field (field_name, Unstructured, v) ->
        show_field ~prefix pp_unstructured field_name v
    | Field.Field (field_name, Content, v) ->
        show_field ~prefix
          Fmt.(const string " " ++ Content_type.pp)
          field_name v
    | Field.Field (field_name, Encoding, v) ->
        show_field ~prefix
          Fmt.(const string " " ++ Content_encoding.pp)
          field_name v
    | Field.Field (_field_name, _, _) -> () in

  List.iteri f lst

let show_body ~tbl:_ ~level:_ _v = Fmt.pr "\n%!"

let show ~fields ~tbl m =
  let up_and_right = Lazy.force up_and_right in
  let rec go ~level (headers, body) =
    show_headers ~fields ~level headers ;
    match body with
    | Some (Mrmime.Mail.Leaf v) -> show_body ~tbl ~level v
    | Some (Mrmime.Mail.Multipart vs) ->
        List.iter
          (fun m ->
            Fmt.pr "%s%s %!" (String.make level ' ') up_and_right ;
            go ~level:(succ level) m)
          vs
    | Some (Mrmime.Mail.Message _) -> assert false
    | None -> Fmt.pr "\n%!" in
  go ~level:0 m

let run _ fields input =
  let ic, close =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  match parser ic with
  | Error (`Msg err) ->
      close ic ;
      `Error (false, Fmt.str "%s." err)
  | Ok (hdrs, mail, tbl) ->
      close ic ;
      show ~fields ~tbl (hdrs, Some mail) ;
      `Ok 0

open Cmdliner
open Args

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
  let doc = "Which fields you want to show." in
  let parser str =
    match String.lowercase_ascii str with
    | "all" -> Ok `All
    | _ ->
        let fields = Astring.String.cuts ~sep:"," str in
        let f acc x =
          match (acc, Field_name.of_string x) with
          | Ok acc, Ok x -> Ok (x :: acc)
          | (Error _ as err), _ -> err
          | Ok _, (Error _ as err) -> err in
        List.fold_left f (Ok []) fields >>| fun vs -> `Some vs in
  let pp ppf = function
    | `All -> Fmt.string ppf "all"
    | `Some lst -> Fmt.Dump.list Field_name.pp ppf lst in
  Arg.(value & opt (conv (parser, pp)) `All & info [ "f"; "fields" ] ~doc)

let cmd =
  let doc = "Describe the structure of the given email." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) describes the structure of the given email like a tree.";
    ] in
  ( Term.(ret (const run $ setup_logs $ fields $ input)),
    Term.info "descr" ~doc ~man )

let () = Term.(exit_status @@ eval cmd)
