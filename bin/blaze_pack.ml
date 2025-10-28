let ( % ) f g = fun x -> f (g x)

let rec clean acc orphans =
  match Miou.care orphans with
  | None | Some None -> Ok acc
  | Some (Some prm) ->
  match Miou.await prm with
  | Ok t -> clean (t :: acc) orphans
  | Error exn as err ->
      Logs.err (fun m ->
          m "got an unexpected error: %s" (Printexc.to_string exn)) ;
      err

let rec terminate acc orphans =
  match Miou.care orphans with
  | None -> Ok acc
  | Some None ->
      Miou.yield () ;
      terminate acc orphans
  | Some (Some prm) ->
  match Miou.await prm with
  | Ok t -> terminate (t :: acc) orphans
  | Error exn as err ->
      Logs.err (fun m ->
          m "got an unexpected error: %s" (Printexc.to_string exn)) ;
      err

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

let load _uid = function
  | Pack.Mail str -> Carton.Value.of_string ~kind:`A str
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

let run_make quiet progress without_progress threads mails_from_stdin
    mails_from_cmdline output =
  Miou_unix.run ~domains:threads @@ fun () ->
  let ( let* ) = Result.bind in
  let mails_from_cmdline = List.map Fpath.v mails_from_cmdline in
  let mails = List.rev_append mails_from_stdin mails_from_cmdline in
  let* mails = parallel ~fn:Pack.filename_to_email mails in
  let* entries = parallel ~fn:Pack.email_to_entries mails in
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
  let oc, finally =
    match output with
    | Some filename ->
        let oc = open_out filename in
        let finally () = close_out oc in
        (oc, finally)
    | None -> (stdout, ignore) in
  Fun.protect ~finally @@ fun () ->
  Seq.iter (output_string oc) pack ;
  Ok ()

let seq_of_filename filename =
  let ic = open_in filename in
  let buf = Bytes.create 0x7ff in
  let dispenser () =
    match input ic buf 0 (Bytes.length buf) with
    | 0 | (exception End_of_file) ->
        close_in ic ;
        None
    | len ->
        let str = Bytes.sub_string buf 0 len in
        Some str in
  Seq.of_dispenser dispenser

let run_list _quit filename =
  Miou_unix.run @@ fun () ->
  let ref_length = Digestif.SHA1.digest_size in
  let seq = seq_of_filename filename in
  let seq =
    let output = De.bigstring_create De.io_buffer_size in
    let allocate bits = De.make_window ~bits in
    Carton.First_pass.of_seq ~output ~allocate ~ref_length ~digest:Pack.sha1 seq
  in
  let pack = Carton_miou_unix.make ~ref_length (Fpath.v filename) in
  let mails_by_offsets = Hashtbl.create 0x100 in
  let mails_by_refs = Hashtbl.create 0x100 in
  let is_a_mail ?offset ?ref () =
    match (offset, ref) with
    | None, None -> false
    | Some offset, _ -> Hashtbl.mem mails_by_offsets offset
    | None, Some ref -> Hashtbl.mem mails_by_refs ref in
  let filter_map = function
    | `Number _ | `Hash _ -> None
    | `Entry { Carton.First_pass.kind = Base `A; offset; size; _ } ->
        Hashtbl.add mails_by_offsets offset () ;
        let blob = Carton.Blob.make ~size in
        let value = Carton.of_offset pack blob ~cursor:offset in
        let uid = Pack.uid_of_value value in
        Hashtbl.add mails_by_refs uid () ;
        Some (offset, value)
    | `Entry { Carton.First_pass.kind = Ofs { sub; _ }; offset; _ } ->
        let parent = offset - sub in
        if is_a_mail ~offset:parent ()
        then (
          Hashtbl.add mails_by_offsets offset () ;
          let size =
            Carton.size_of_offset pack ~cursor:offset Carton.Size.zero in
          let blob = Carton.Blob.make ~size in
          let value = Carton.of_offset pack blob ~cursor:offset in
          let uid = Pack.uid_of_value value in
          Hashtbl.add mails_by_refs uid () ;
          Some (offset, value))
        else None
    | `Entry { Carton.First_pass.kind = Ref { ptr; _ }; offset; _ } ->
        if is_a_mail ~ref:ptr ()
        then (
          Hashtbl.add mails_by_offsets offset () ;
          let size =
            Carton.size_of_offset pack ~cursor:offset Carton.Size.zero in
          let blob = Carton.Blob.make ~size in
          let value = Carton.of_offset pack blob ~cursor:offset in
          let uid = Pack.uid_of_value value in
          Hashtbl.add mails_by_refs uid () ;
          Some (offset, value))
        else None
    | _ -> None in
  let seq = Seq.filter_map filter_map seq in
  let show (offset, value) =
    assert (Carton.Value.kind value = `A) ;
    let bstr = Carton.Value.bigstring value in
    let bstr = Bigarray.Array1.sub bstr 0 (Carton.Value.length value) in
    let uid = Pack.uid_of_value value in
    match Email.of_bigstring bstr with
    | Ok _t -> Fmt.pr "%08x %a\n%!" offset Carton.Uid.pp uid
    | Error (`Msg msg) -> Fmt.failwith "%s" msg in
  Seq.iter show seq

let entries_of_pack cfg pack =
  let matrix, hash = Pack.verify_from_pack ~cfg pack in
  let fn _idx = function
    | Carton.Unresolved_base _ | Carton.Unresolved_node _ ->
        Logs.err (fun m -> m "object %d unresolved" _idx) ;
        assert false
    | Resolved_base { cursor; uid; crc; _ } ->
        let uid = Classeur.unsafe_uid_of_string (uid :> string) in
        Classeur.Encoder.{ uid; crc; offset = Int64.of_int cursor }
    | Resolved_node { cursor; uid; crc; _ } ->
        let uid = Classeur.unsafe_uid_of_string (uid :> string) in
        Classeur.Encoder.{ uid; crc; offset = Int64.of_int cursor } in
  (Array.mapi fn matrix, hash)

let run_index _quiet threads filename =
  Miou_unix.run ~domains:threads @@ fun () ->
  let cfg = Pack.config ~threads () in
  let entries, hash = entries_of_pack cfg (Fpath.v filename) in
  let encoder =
    let digest = Pack.sha1 in
    let ref_length = Digestif.SHA1.digest_size in
    Classeur.Encoder.encoder `Manual ~digest ~pack:hash ~ref_length entries
  in
  let output = Fpath.set_ext ".idx" (Fpath.v filename) in
  let oc = open_out (Fpath.to_string output) in
  let finally () = close_out oc in
  Fun.protect ~finally @@ fun () ->
  let out = Bytes.create 0x7ff in
  let rec go (`Await as await) =
    match Classeur.Encoder.encode encoder await with
    | `Ok ->
        let len = Bytes.length out - Classeur.Encoder.dst_rem encoder in
        output_substring oc (Bytes.unsafe_to_string out) 0 len
    | `Partial ->
        let len = Bytes.length out - Classeur.Encoder.dst_rem encoder in
        output_substring oc (Bytes.unsafe_to_string out) 0 len ;
        Classeur.Encoder.dst encoder out 0 (Bytes.length out) ;
        go await in
  Classeur.Encoder.dst encoder out 0 (Bytes.length out) ;
  go `Await

let run_get _quiet idx identifier =
  Miou_unix.run @@ fun () ->
  let pack = Fpath.set_ext ".pack" idx in
  let ref_length = Digestif.SHA1.digest_size in
  let idx = Pack.index idx in
  let index (uid : Carton.Uid.t) =
    let uid = Classeur.uid_of_string_exn idx (uid :> string) in
    Carton.Local (Classeur.find_offset idx uid) in
  let pack = Carton_miou_unix.make ~ref_length ~index pack in
  let size =
    match identifier with
    | `Offset cursor -> Carton.size_of_offset pack ~cursor Carton.Size.zero
    | `Uid uid ->
        let uid = Carton.Uid.unsafe_of_string uid in
        Carton.size_of_uid pack ~uid Carton.Size.zero in
  let blob = Carton.Blob.make ~size in
  let value =
    match identifier with
    | `Offset cursor -> Carton.of_offset pack blob ~cursor
    | `Uid uid ->
        let uid = Carton.Uid.unsafe_of_string uid in
        Carton.of_uid pack blob ~uid in
  match Carton.Value.kind value with
  | `B | `C | `D ->
      let pp_identifier ppf = function
        | `Offset cursor -> Fmt.pf ppf "cursor:%08x" cursor
        | `Uid uid -> Fmt.pf ppf "%s" (Ohex.encode uid) in
      Fmt.failwith "Invalid object %a, it is not an email" pp_identifier
        identifier
  | `A -> (
      let str = Carton.Value.string value in
      match Email.of_string str with
      | Error (`Msg msg) -> Fmt.failwith "%s" msg
      | Ok (t, _) ->
          let load uid =
            let uid = Carton.Uid.unsafe_of_string uid in
            let size = Carton.size_of_uid pack ~uid Carton.Size.zero in
            let blob = Carton.Blob.make ~size in
            let value = Carton.of_uid pack blob ~uid in
            let len = Carton.Value.length value in
            let bstr = Carton.Value.bigstring value in
            Bigarray.Array1.sub bstr 0 len in
          let seq = Email.to_seq ~load t in
          let fn = function
            | `String str -> output_string stdout str
            | `Value bstr -> Email.output_bigstring stdout bstr in
          Seq.iter fn seq)

let pp_status tbl ~max_consumed ~max_offset ppf = function
  | Carton.Unresolved_base { cursor } ->
      Fmt.pf ppf "%08x %d" cursor (Hashtbl.find tbl cursor)
  | Unresolved_node _ -> ()
  | Resolved_base { cursor; uid; kind; crc } ->
      Fmt.pf ppf "%a %a %*d %*d    %08lx" Carton.Uid.pp uid Carton.Kind.pp kind
        max_offset cursor max_consumed (Hashtbl.find tbl cursor)
        (Optint.to_unsigned_int32 crc)
  | Resolved_node { cursor; uid; kind; depth; parent; crc } ->
      Fmt.pf ppf "%a %a %*d %*d %2d %08lx %a" Carton.Uid.pp uid Carton.Kind.pp
        kind max_offset cursor max_consumed (Hashtbl.find tbl cursor) (depth - 1)
        (Optint.to_unsigned_int32 crc)
        Carton.Uid.pp parent

let with_reporter ~config quiet t fn =
  let on_entry, on_object, finally =
    match quiet with
    | true -> (ignore, ignore, ignore)
    | false ->
        let lines = Progress.Multi.(line t ++ line t) in
        let display = Progress.Display.start ~config lines in
        let[@warning "-8"] Progress.Reporter.[ reporter0; reporter1 ] =
          Progress.Display.reporters display in
        let on_entry n =
          reporter0 n ;
          Progress.Display.tick display in
        let on_object n =
          reporter1 n ;
          Progress.Display.tick display in
        let finally () = Progress.Display.finalise display in
        (on_entry, on_object, finally) in
  fn (on_entry, on_object, finally)

let printer ~total entries objects progress =
  let rec go total counter =
    if counter < total * 2
    then begin
      let new_entries = Atomic.exchange entries 0 in
      let new_objects = Atomic.exchange objects 0 in
      let on_entry, on_object, _ = Miou.Lazy.force progress in
      on_entry new_entries ;
      on_object new_objects ;
      Miou.yield () ;
      go total (counter + new_entries + new_objects)
    end in
  fun () ->
    let total = Miou.Computation.await_exn total in
    go total 0

let display ~config quiet t =
  let c = Miou.Computation.create () in
  let consumed = Hashtbl.create 0x7ff in
  let entries = Atomic.make 0 in
  let objects = Atomic.make 0 in
  let progress =
    Miou.Lazy.from_fun @@ fun () ->
    let total = Miou.Computation.await_exn c in
    with_reporter ~config quiet (t ~total) Fun.id in
  let on_entry ~max entry =
    if Miou.Computation.is_running c
    then assert (Miou.Computation.try_return c max) ;
    Hashtbl.add consumed entry.Carton_miou_unix.offset entry.consumed ;
    Atomic.incr entries ;
    Miou.yield () in
  let on_object ~cursor:_ _ _ =
    Atomic.incr objects ;
    Miou.yield () in
  let printer = Miou.async (printer ~total:c entries objects progress) in
  let finally () =
    let _, _, finally = Miou.Lazy.force progress in
    finally () ;
    Miou.cancel printer in
  (on_entry, on_object, finally, consumed)

let run_verify quiet progress without_progress threads pagesize pack =
  Miou_unix.run ~domains:threads @@ fun () ->
  let on_entry, on_object, finally, tbl =
    display ~config:progress (quiet || without_progress) bar in
  let cfg = Pack.config ~threads ~pagesize ~on_entry ~on_object () in
  let matrix, _hash =
    Fun.protect ~finally @@ fun () ->
    match pack with
    | `Pack filename -> Pack.verify_from_pack ~cfg filename
    | `Idx (filename, _) -> Pack.verify_from_idx ~cfg filename in
  let max_consumed = Hashtbl.fold (fun _ a b -> Int.max a b) tbl 0 in
  let max_consumed = Float.to_int (log10 (Float.of_int max_consumed)) + 1 in
  let max_offset =
    match matrix.(Array.length matrix - 1) with
    | Carton.Unresolved_base { cursor } -> cursor
    | Unresolved_node _ -> 0
    | Resolved_base { cursor; _ } -> cursor
    | Resolved_node { cursor; _ } -> cursor in
  let max_offset = Float.to_int (log10 (Float.of_int max_offset)) + 1 in
  let pp_status = pp_status tbl ~max_consumed ~max_offset in
  if not quiet then Array.iter (Fmt.pr "%a\n%!" pp_status) matrix

open Cmdliner
open Args

let setup_mails_from_in_channel ic =
  let rec go mails =
    match input_line ic with
    | exception End_of_file -> List.rev mails
    | filename ->
    match Fpath.of_string filename with
    | Ok value ->
        if Sys.file_exists filename && Sys.is_directory filename = false
        then go (value :: mails)
        else
          let () =
            Logs.warn (fun m -> m "%a does not exist, ignore it" Fpath.pp value)
          in
          go mails
    | Error _ ->
        Logs.warn (fun m -> m "%S is not a valid filename, ignore it" filename) ;
        go mails in
  go []

let mails =
  let doc = "Emails to encode into a PACK file." in
  let open Arg in
  value & opt_all Args.file [] & info [ "m"; "mail" ] ~doc ~docv:"MAIL"

let list_of_mails =
  let doc = "A file (or $(i,stdin)) containing a list of emails." in
  let open Arg in
  value & pos 0 Args.file "-" & info [] ~doc ~docv:"FILE"

let setup_mails_from_in_channel = function
  | "-" -> setup_mails_from_in_channel stdin
  | filename ->
      let ic = open_in filename in
      let finally () = close_in ic in
      Fun.protect ~finally @@ fun () -> setup_mails_from_in_channel ic

let setup_mails_from_in_channel =
  let open Term in
  const setup_mails_from_in_channel $ list_of_mails

let output =
  let doc = "The output file where to save the PACK file." in
  let open Arg in
  value
  & opt (some non_existing_file) None
  & info [ "o"; "output" ] ~doc ~docv:"FILE"

let pack =
  let doc = "The PACKv2 file." in
  let open Arg in
  required & pos 0 (some file) None & info [] ~doc ~docv:"PACK"

let uid_of_string_opt str =
  match Ohex.decode ~skip_whitespace:true str with
  | uid -> Some uid
  | exception _exn -> None

let identifier =
  let doc =
    "The unique identifier of the object or the offset of it into the given \
     PACK file." in
  let parser str =
    match (int_of_string_opt str, uid_of_string_opt str) with
    | Some offset, None -> Ok (`Offset offset)
    | _, Some uid -> Ok (`Uid uid)
    | None, None -> error_msgf "Invalid position of the object: %S" str in
  let pp ppf = function
    | `Offset offset -> Fmt.pf ppf "0x%x" offset
    | `Uid uid -> Fmt.pf ppf "%s" (Ohex.encode uid) in
  let identifier = Arg.conv (parser, pp) in
  let open Arg in
  required & pos 1 (some identifier) None & info [] ~doc ~docv:"IDENTIFIER"

let make_term =
  let open Term in
  let to_ret = function
    | Ok () -> `Ok ()
    | Error exn -> `Error (false, Printexc.to_string exn) in
  let term =
    const run_make
    $ setup_logs
    $ setup_progress
    $ without_progress
    $ threads ~min:2 ()
    $ setup_mails_from_in_channel
    $ mails
    $ output in
  let term = map to_ret term in
  ret term

let list_term =
  let open Term in
  const run_list $ setup_logs $ pack

let index_term =
  let open Term in
  const run_index $ setup_logs $ threads () $ pack

let pack =
  let parser str =
    match Fpath.of_string str with
    | Ok value when Sys.file_exists str && Sys.is_directory str = false -> begin
        match Fpath.get_ext value with
        | ".pack" -> Ok (`Pack value)
        | ".idx" ->
            let pack = Fpath.set_ext ".pack" value in
            if
              Sys.file_exists (Fpath.to_string pack)
              && Sys.is_directory (Fpath.to_string pack) = false
            then Ok (`Idx (value, pack))
            else
              error_msgf "The associated PACK file to %a does not exist"
                Fpath.pp value
        | _ -> error_msgf "Unexpected file %a" Fpath.pp value
      end
    | Ok value -> error_msgf "%a does not exist" Fpath.pp value
    | Error _ as err -> err in
  let pp ppf = function `Pack value | `Idx (value, _) -> Fpath.pp ppf value in
  Arg.conv (parser, pp) ~docv:"FILE"

let errorf ?(usage = false) fmt = Fmt.kstr (fun msg -> `Error (usage, msg)) fmt

let get_term =
  let setup_pack = function
    | `Pack pack ->
        let idx = Fpath.set_ext ".idx" pack in
        if
          Sys.file_exists (Fpath.to_string idx)
          && Sys.is_directory (Fpath.to_string idx) = false
        then `Ok idx
        else
          errorf "You must have the IDX file of %a to be able to get emails."
            Fpath.pp pack
    | `Idx (idx, _) -> `Ok idx in
  let open Term in
  let pack =
    let doc =
      "The file used to access to the PACK file (it can be the PACK file \
       directly or the associated IDX file). The IDX file must exist." in
    Arg.(required & pos 0 (some pack) None & info [] ~doc ~docv:"FILE") in
  let setup_pack = ret (const setup_pack $ pack) in
  const run_get $ setup_logs $ setup_pack $ identifier

external getpagesize : unit -> int = "carton_miou_unix_getpagesize" [@@noalloc]

let pagesize =
  let doc =
    "The memory page size to use to verify the given PACK file (must be a \
     power of two)." in
  let open Arg in
  value & opt int (getpagesize ()) & info [ "pagesize" ] ~doc ~docv:"BYTES"

let pack =
  let doc =
    "The file used to access to the PACK file (it can be the PACK file \
     directly or the associated IDX file)." in
  Arg.(required & pos 0 (some pack) None & info [] ~doc ~docv:"FILE")

let verify_term =
  let open Term in
  const run_verify
  $ setup_logs
  $ setup_progress
  $ without_progress
  $ threads ()
  $ pagesize
  $ pack

let error_to_string exn = Printexc.to_string exn

let make_cmd =
  let doc = "A tool to pack a bunch of emails into a PACKv2 file." in
  let man = [] in
  let info = Cmd.info "make" ~doc ~man in
  Cmd.v info make_term

let list_cmd =
  let doc = "A tool to list emails from the given PACKv2 file." in
  let man = [] in
  let info = Cmd.info "list" ~doc ~man in
  Cmd.v info list_term

let index_cmd =
  let doc = "A tool to generate the IDX file from a PACK file." in
  let man = [] in
  let info = Cmd.info "index" ~doc ~man in
  Cmd.v info index_term

let get_cmd =
  let doc = "A tool to get an email from a PACK file." in
  let man = [] in
  let info = Cmd.info "get" ~doc ~man in
  Cmd.v info get_term

let verify_cmd =
  let doc = "A tool to verify a PACK file of emails." in
  let man = [] in
  let info = Cmd.info "verify" ~doc ~man in
  Cmd.v info verify_term

let default = Term.(ret (const (`Help (`Pager, None))))

let cmd =
  let doc = "A tool to manipulate PACKv2 which contains emails." in
  let man = [] in
  Cmd.group ~default
    (Cmd.info "pack" ~doc ~man)
    [ make_cmd; list_cmd; index_cmd; get_cmd; verify_cmd ]
