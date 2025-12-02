let to_document pack offset =
  match Carton.kind_of_offset pack ~cursor:offset with
  | `A | `B | `D -> None
  | `C -> begin
      let zero = Carton.Size.zero in
      let size = Carton.size_of_offset pack ~cursor:offset zero in
      let blob = Carton.Blob.make ~size in
      let value = Carton.of_offset pack blob ~cursor:offset in
      let str = Carton.Value.string value in
      match Stem.of_string str with
      | Ok (uid, blob, length, tokens) ->
          let uid = Classeur.unsafe_uid_of_string uid
          and blob = Carton.Uid.unsafe_of_string blob in
          let guid = (uid, blob) in
          Some (guid, `Document (Bm25.document ~length ~tokens guid))
      | Error _ -> None
    end

let threads (pack, idx, queue) =
  let idx = Pack.index idx in
  let index (uid : Carton.Uid.t) =
    let uid = Classeur.uid_of_string_exn idx (uid :> string) in
    let offset = Classeur.find_offset idx uid in
    Logs.debug (fun m -> m "%s -> %08x" (Ohex.encode (uid :> string)) offset) ;
    Carton.Local offset in
  let pack = Pack.make ~index pack in
  let rec go acc =
    match Flux.Bqueue.get queue with
    | Some offset ->
        let entry =
          try to_document pack offset
          with exn ->
            Logs.err (fun m ->
                m "Unexpected error (for %08x): %s" offset
                  (Printexc.to_string exn)) ;
            None in
        Option.fold ~none:acc ~some:(fun entry -> entry :: acc) entry |> go
    | None -> acc in
  go []

let producer idx queue =
  let idx = Pack.index idx in
  let fn ~uid:_ ~crc:_ ~offset =
    Logs.debug (fun m -> m "Produce %08x" offset) ;
    Flux.Bqueue.put queue offset in
  Classeur.iter ~fn idx ;
  Flux.Bqueue.close queue ;
  Logs.debug (fun m -> m "Offsets sent!")

let run domains _quiet bm25 (pack, idx, _stem) query =
  Miou_unix.run ~domains @@ fun () ->
  let queue = Flux.Bqueue.(create with_close) 0x7ff in
  let producer = Miou.async @@ fun () -> producer idx queue in
  let lst = List.init (Miou.Domain.available ()) (fun _ -> (pack, idx, queue)) in
  let consumers = Miou.parallel threads lst in
  let documents =
    List.map (function Ok docs -> docs | Error exn -> raise exn) consumers in
  let documents = List.flatten documents in
  Miou.await_exn producer ;
  let t = Bm25.make ~cfg:bm25 documents in
  let lst = Bm25.score t query in
  let lst = List.sort (fun (_, a) (_, b) -> Float.compare b a) lst in
  let show (((uid : Classeur.uid), _), score) =
    if score > 0.0 then Fmt.pr "%s: %f\n%!" (Ohex.encode (uid :> string)) score
  in
  List.iter show lst

open Cmdliner
open Blaze_cli

let errorf ?(usage = true) fmt = Fmt.kstr (fun err -> `Error (usage, err)) fmt

let setup_pack filename =
  let exists ~ext t =
    let t = Fpath.(to_string (set_ext ~multi:false ext t)) in
    Sys.file_exists t && Sys.is_regular_file t in
  let idx_exists = exists ~ext:".idx"
  and pack_exists = exists ~ext:".pack"
  and stem_exists = exists ~ext:".stem" in
  let to_idx = Fpath.set_ext ~multi:false ".idx"
  and to_pack = Fpath.set_ext ~multi:false ".pack"
  and to_stem_if_exists t =
    if stem_exists t then Some (Fpath.set_ext ~multi:false ".stem" t) else None
  in
  match Fpath.get_ext filename with
  | ".pack" when idx_exists filename ->
      `Ok (filename, to_idx filename, to_stem_if_exists filename)
  | ".idx" when pack_exists filename ->
      `Ok (to_pack filename, filename, to_stem_if_exists filename)
  | ".stem" when idx_exists filename && pack_exists filename ->
      `Ok (to_pack filename, to_idx filename, Some filename)
  | ".pack" | ".idx" | ".stem" ->
      let basename = Fpath.basename (Fpath.rem_ext filename) in
      errorf ~usage:false
        "Missing a file from an archive (%s.pack and %s.idx must exist)"
        basename basename
  | _ -> errorf ~usage:true "Invalid extension of %a" Fpath.pp filename

let existing_file =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str && Sys.is_regular_file str -> Ok v
    | Ok v -> error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  let existing_file = Arg.conv (parser, Fpath.pp) in
  let doc = "The $(i,blaze) archive (*.pack or *.idx, both are required)." in
  let open Arg in
  required & pos 0 (some existing_file) None & info [] ~doc ~docv:"FILENAME"

let setup_pack =
  let open Term in
  ret (const setup_pack $ existing_file)

let setup_bm25 parallel encoding actions language =
  Bm25.config ~parallel ~encoding ~actions language

let parallel =
  let doc =
    "With this option, calculation of occurrences is decoupled from the \
     $(i,tokenisation) of a document." in
  Arg.(value & flag & info [ "parallel" ] ~doc)

let setup_bm25 =
  let open Term in
  const setup_bm25 $ parallel $ encoding $ actions $ language

let query =
  let doc = "The sentence to search into the given $(i,blaze) archive." in
  let open Arg in
  required & pos 1 (some string) None & info [] ~doc ~docv:"QUERY"

let min = Int.min 4 (Stdlib.Domain.recommended_domain_count () - 1)

let cmd =
  let doc = "The Okapi search engine applied into a $(i,blaze) archive." in
  let man = [] in
  let info = Cmd.info "okapi" ~doc ~man in
  let term =
    let open Term in
    const run $ threads ~min () $ setup_logs $ setup_bm25 $ setup_pack $ query
  in
  Cmd.v info term
