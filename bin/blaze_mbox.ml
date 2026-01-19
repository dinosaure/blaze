let ( $ ) f g = fun x -> f (g x)

let parallel ~fn lst =
  let domains = Miou.Domain.available () in
  let chop len lst =
    let rec go acc n lst =
      if n <= 0
      then (acc, lst)
      else match lst with [] -> (acc, []) | x :: r -> go (x :: acc) (n - 1) r
    in
    go [] len lst in
  let rec go acc lst =
    match (acc, lst) with
    | (Error _ as err), _ -> err
    | Ok acc, [] -> Ok acc
    | Ok acc, lst ->
        let todo, lst = chop domains lst in
        let results = Miou.parallel fn todo in
        let rec check acc = function
          | [] -> go (Ok acc) lst
          | Ok value :: rest -> check (value :: acc) rest
          | (Error _ as err) :: _ -> err in
        check acc results in
  go (Ok []) lst

let load _uid = function
  | Pack.Stem (_uid, _blob, _length, _tbl) -> assert false (* TODO *)
  | Pack.Mail str -> Carton.Value.of_string ~kind:`A str
  | Pack.Tree str -> Carton.Value.of_string ~kind:`D str
  | Pack.Body (filename, pos, len) ->
      let fd =
        Unix.openfile (Fpath.to_string filename) Unix.[ O_RDONLY ] 0o644 in
      let finally () = Unix.close fd in
      Fun.protect ~finally @@ fun () ->
      let barr =
        Unix.map_file fd ~pos:(Int64.of_int pos) Bigarray.char Bigarray.c_layout
          false [| len |] in
      let bstr = Bigarray.array1_of_genarray barr in
      Carton.Value.make ~kind:`B bstr

let delete_duplicates ?(quiet = true) entriess =
  let tbl = Hashtbl.create 0x100 in
  let cnt = ref 0 in
  let rec go acc = function
    | [] -> List.rev acc
    | entry :: rest -> (
        let hash = Cartonnage.Entry.uid entry in
        match Hashtbl.find tbl hash with
        | _ ->
            incr cnt ;
            go acc rest
        | exception Not_found ->
            Hashtbl.add tbl hash () ;
            go (entry :: acc) rest) in
  if (not quiet) && !cnt > 0 then Fmt.pr "%d duplicate entries\n%!" !cnt ;
  List.fold_left (fun acc entries -> go [] entries :: acc) [] entriess

let explode ?tmp:temp_dir seq =
  let producer seq =
    let rec go mails seq actual =
      match (actual, Seq.uncons seq) with
      | None, Some (`From _, seq) ->
          let tmp = Filename.temp_file ?temp_dir "blaze-" ".eml" in
          let oc = open_out tmp in
          let tmp = Fpath.v tmp in
          go (tmp :: mails) seq (Some (tmp, oc))
      | Some (tmp, oc), Some (`Line line, seq) ->
          output_string oc line ;
          go mails seq (Some (tmp, oc))
      | None, Some (`Line line, _) ->
          Logs.err (fun m -> m "Unexpected line: %S" line) ;
          assert false
      | Some (_, oc), Some (`From _, seq) ->
          close_out oc ;
          let tmp = Filename.temp_file ?temp_dir "blaze-" ".eml" in
          let oc = open_out tmp in
          let tmp = Fpath.v tmp in
          go (tmp :: mails) seq (Some (tmp, oc))
      | None, None -> mails
      | Some (_, oc), None ->
          close_out oc ;
          mails in
    go [] seq None in
  producer seq

let bar ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  list [ brackets @@ bar ~style ~width:(`Fixed 30) total; count_to total ]

let with_reporter ~config ?total quiet =
  match (quiet, total) with
  | true, _ | _, None -> (ignore, ignore)
  | false, Some total ->
      let display = Progress.(Display.start ~config Multi.(line (bar ~total))) in
      let[@warning "-8"] Progress.Reporter.[ reporter ] =
        Progress.Display.reporters display in
      let on n =
        reporter n ;
        Progress.Display.tick display in
      let finally () = Progress.Display.finalise display in
      (on, finally)

let run_pack quiet progress without_progress threads mbox output =
  Miou_unix.run ~domains:threads @@ fun () ->
  let ( let* ) = Result.bind in
  let ic, ic_finally =
    match mbox with
    | "-" -> (stdin, ignore)
    | filename ->
        let ic = open_in filename in
        let finally () = close_in ic in
        (ic, finally) in
  Fun.protect ~finally:ic_finally @@ fun () ->
  let seq = Mbox.of_in_channel ic in
  let mails = explode seq in
  let* mails = parallel ~fn:Pack.filepath_to_email mails in
  let fn (filepath, t, _) = Pack.email_to_entries filepath t in
  let* entries = parallel ~fn mails in
  let entries = delete_duplicates ~quiet entries in
  let with_header =
    List.fold_left (fun acc entries -> acc + List.length entries) 0 entries
  in
  let entries = List.map List.to_seq entries in
  let entries = List.to_seq entries in
  let entries = Seq.concat entries in
  let targets = Pack.delta ~load entries in
  let with_signature = Digestif.SHA1.empty in
  let on, finally =
    with_reporter ~config:progress ~total:with_header (quiet || without_progress)
  in
  Fun.protect ~finally @@ fun () ->
  let targets =
    Seq.map
      (fun value ->
        on 1 ;
        value)
      targets in
  let pack = Pack.to_pack ~with_header ~with_signature ~load targets in
  let oc, oc_finally =
    match output with
    | Some filename ->
        let oc = open_out filename in
        let finally () = close_out oc in
        (oc, finally)
    | None -> (stdout, ignore) in
  Fun.protect ~finally:oc_finally @@ fun () ->
  Seq.iter (output_string oc) pack ;
  Ok ()

open Cmdliner
open Blaze_cli

let mbox =
  let doc = "The mbox file to manipulate." in
  let open Arg in
  value & pos 0 Blaze_cli.file_or_stdin "-" & info [] ~doc ~docv:"FILE"

let output =
  let doc = "The output file where to save the PACK file." in
  let open Arg in
  value
  & opt (some non_existing_file) None
  & info [ "o"; "output" ] ~doc ~docv:"FILE"

let pack_term =
  let open Term in
  let to_result = function
    | Ok () -> `Ok ()
    | Error exn -> `Error (false, Fmt.str "%s." (Printexc.to_string exn)) in
  let term =
    const run_pack
    $ setup_logs
    $ setup_progress
    $ without_progress
    $ threads ~min:2 ()
    $ mbox
    $ output in
  ret (const to_result $ term)

let cmd =
  let doc = "Transform a mbox file to a PACK file." in
  let man = [] in
  let info = Cmd.info "mbox" ~doc ~man in
  Cmd.v info pack_term
