let to_value mail =
  match Email.of_filename mail with
  | Ok t -> (mail, t)
  | Error (`Msg msg) ->
      Logs.err (fun m -> m "%a is an invalid email" Fpath.pp mail) ;
      Fmt.failwith "%s" msg

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
  let rec go acc orphans lst =
    match (acc, lst) with
    | (Error _ as err), _ -> err
    | Ok acc, [] -> terminate acc orphans
    | Ok acc, x :: r ->
        let acc = clean acc orphans in
        let _ = Miou.call ~orphans @@ fun () -> fn x in
        go acc orphans r in
  go (Ok []) (Miou.orphans ()) lst

type src = Mail of string | Body of Fpath.t * int * int

let pp_kind ppf = function
  | `A -> Fmt.string ppf "mail"
  | `B -> Fmt.string ppf "blob"
  | `C | `D -> Fmt.string ppf "deadbeef"

let mail_identify ~kind ?(off = 0) ?len bstr =
  let len =
    match len with Some len -> len | None -> Bigarray.Array1.dim bstr - off
  in
  let ctx = Digestif.SHA1.empty in
  let hdr = Fmt.str "%a %d\000" pp_kind kind len in
  let ctx = Digestif.SHA1.feed_string ctx hdr in
  let ctx = Digestif.SHA1.feed_bigstring ctx ~off ~len bstr in
  let hash = Digestif.SHA1.get ctx in
  let hash = Digestif.SHA1.to_raw_string hash in
  Carton.Uid.unsafe_of_string hash

let to_entries (filename, t) =
  let open Cartonnage in
  let fd = Unix.openfile (Fpath.to_string filename) Unix.[ O_RDONLY ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let fn (pos, pos_end) =
    let len = pos_end - pos in
    let barr =
      Unix.map_file fd ~pos:(Int64.of_int pos) Bigarray.char Bigarray.c_layout
        false [| len |] in
    let bstr = Bigarray.array1_of_genarray barr in
    let hash = mail_identify ~kind:`B bstr in
    (pos, len, hash) in
  let t = Email.map fn t in
  let fn entries (pos, len, hash) =
    let entry =
      Entry.make ~kind:`B ~length:len hash (Body (filename, pos, len)) in
    entry :: entries in
  let entries = Email.fold fn [] t in
  let t = Email.map (fun (_, _, (hash : Carton.Uid.t)) -> (hash :> string)) t in
  let serialized = Email.to_string t in
  let hash =
    let hdr = Fmt.str "mail %d\000" (String.length serialized) in
    Digestif.SHA1.digest_string (hdr ^ serialized) in
  let hash = Digestif.SHA1.to_raw_string hash in
  let hash = Carton.Uid.unsafe_of_string hash in
  let entry =
    Entry.make ~kind:`A ~length:(String.length serialized) hash
      (Mail serialized) in
  entry :: entries

let load _uid = function
  | Mail str -> Carton.Value.of_string ~kind:`A str
  | Body (filename, pos, len) ->
      let fd =
        Unix.openfile (Fpath.to_string filename) Unix.[ O_RDONLY ] 0o644 in
      let finally () = Unix.close fd in
      Fun.protect ~finally @@ fun () ->
      let barr =
        Unix.map_file fd ~pos:(Int64.of_int pos) Bigarray.char Bigarray.c_layout
          false [| len |] in
      let bstr = Bigarray.array1_of_genarray barr in
      Carton.Value.make ~kind:`B bstr

let sha1 =
  let module Hash = (val Digestif.module_of_hash' `SHA1) in
  let feed_bigstring bstr ctx = Hash.feed_bigstring ctx bstr in
  let feed_bytes buf ~off ~len ctx = Hash.feed_bytes ctx ~off ~len buf in
  let hash =
    {
      Carton.First_pass.feed_bytes;
      feed_bigstring;
      serialize = Fun.compose Hash.to_raw_string Hash.get;
      length = Hash.digest_size;
    } in
  Carton.First_pass.Digest (hash, Hash.empty)

let run_make _quiet mails_from_stdin mails_from_cmdline output =
  Miou_unix.run @@ fun () ->
  let ( let* ) = Result.bind in
  let ref_length = Digestif.SHA1.digest_size in
  let mails = List.rev_append mails_from_stdin mails_from_cmdline in
  let* mails = parallel ~fn:to_value mails in
  let* entries = parallel ~fn:to_entries mails in
  let with_header =
    List.fold_left (fun acc entries -> acc + List.length entries) 0 entries
  in
  let entries = List.map List.to_seq entries in
  let entries = List.to_seq entries in
  let entries = Seq.concat entries in
  let targets = Carton_miou_unix.delta ~ref_length ~load entries in
  let pack =
    Carton_miou_unix.to_pack ~with_header ~with_signature:sha1 ~load targets
  in
  let oc, finally =
    match output with
    | Some filename ->
        let oc = open_out (Fpath.to_string filename) in
        let finally () = close_out oc in
        (oc, finally)
    | None -> (stdout, ignore) in
  Fun.protect ~finally @@ fun () ->
  Seq.iter (output_string oc) pack ;
  Ok ()

let seq_of_filename filename =
  let ic = open_in (Fpath.to_string filename) in
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
    Carton.First_pass.of_seq ~output ~allocate ~ref_length ~digest:sha1 seq
  in
  let pack = Carton_miou_unix.make ~ref_length filename in
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
        let uid =
          mail_identify ~kind:`A
            ~len:(Carton.Value.length value)
            (Carton.Value.bigstring value) in
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
          let uid =
            mail_identify ~kind:`A
              ~len:(Carton.Value.length value)
              (Carton.Value.bigstring value) in
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
          let uid =
            mail_identify ~kind:`A
              ~len:(Carton.Value.length value)
              (Carton.Value.bigstring value) in
          Hashtbl.add mails_by_refs uid () ;
          Some (offset, value))
        else None
    | _ -> None in
  let seq = Seq.filter_map filter_map seq in
  let show (offset, value) =
    assert (Carton.Value.kind value = `A) ;
    let bstr = Carton.Value.bigstring value in
    let bstr = Bigarray.Array1.sub bstr 0 (Carton.Value.length value) in
    let uid = mail_identify ~kind:`A ~len:(Carton.Value.length value) bstr in
    match Email.of_bigstring bstr with
    | Ok _t -> Fmt.pr "%08x %a\n%!" offset Carton.Uid.pp uid
    | Error (`Msg msg) -> Fmt.failwith "%s" msg in
  Seq.iter show seq

let entries_of_pack cfg digest pack =
  let matrix, hash = Carton_miou_unix.verify_from_pack ~cfg ~digest pack in
  let fn _idx = function
    | Carton.Unresolved_base _ | Carton.Unresolved_node ->
        Logs.err (fun m -> m "object %d unresolved" _idx) ;
        assert false
    | Resolved_base { cursor; uid; crc; _ } ->
        let uid = Classeur.unsafe_uid_of_string (uid :> string) in
        Classeur.Encoder.{ uid; crc; offset = Int64.of_int cursor }
    | Resolved_node { cursor; uid; crc; _ } ->
        let uid = Classeur.unsafe_uid_of_string (uid :> string) in
        Classeur.Encoder.{ uid; crc; offset = Int64.of_int cursor } in
  (Array.mapi fn matrix, hash)

let run_index _quiet threads pack =
  Miou_unix.run ~domains:threads @@ fun () ->
  let ref_length = Digestif.SHA1.digest_size in
  let cfg = Carton_miou_unix.config ~threads ~ref_length mail_identify in
  let entries, hash = entries_of_pack cfg sha1 pack in
  let encoder =
    Classeur.Encoder.encoder `Manual ~digest:sha1 ~pack:hash ~ref_length entries
  in
  let output = Fpath.set_ext ".idx" pack in
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

let run_get _quiet pack idx identifier =
  Miou_unix.run @@ fun () ->
  let pack =
    match pack with
    | Some pack -> Some pack
    | None ->
        let pack = Fpath.set_ext ".pack" idx in
        if
          Sys.file_exists (Fpath.to_string pack) = false
          || Sys.is_directory (Fpath.to_string pack)
        then None
        else Some pack in
  if Option.is_none pack then Fmt.failwith "PACK file not found" ;
  let ref_length = Digestif.SHA1.digest_size in
  let pack = Option.get pack in
  let idx =
    Carton_miou_unix.index ~hash_length:Digestif.SHA1.digest_size ~ref_length
      idx in
  let index (uid : Carton.Uid.t) =
    let uid = Classeur.uid_of_string_exn idx (uid :> string) in
    Classeur.find_offset idx uid in
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
      | Ok t ->
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

let existing_file =
  let parser str =
    match Fpath.of_string str with
    | Ok value ->
        if Sys.file_exists str && Sys.is_directory str = false
        then Ok value
        else error_msgf "%a does not exists" Fpath.pp value
    | Error _ as err -> err in
  Arg.conv ~docv:"MAIL" (parser, Fpath.pp)

let mails =
  let doc = "Emails to encode into a PACK file." in
  let open Arg in
  value & opt_all existing_file [] & info [ "m"; "mail" ] ~doc ~docv:"MAIL"

let list_of_mails =
  let doc = "A file (or $(i,stdin)) containing a list of emails." in
  let parser = function
    | "-" -> Ok None
    | str ->
    match Fpath.of_string str with
    | Ok value ->
        if Sys.file_exists str && Sys.is_directory str = false
        then Ok (Some value)
        else error_msgf "%a does not exists" Fpath.pp value
    | Error _ as err -> err in
  let existing_file =
    let pp ppf = function
      | None -> Fmt.string ppf "-"
      | Some filename -> Fpath.pp ppf filename in
    Arg.conv ~docv:"FILE" (parser, pp) in
  let open Arg in
  value & pos 0 (some existing_file) None & info [] ~doc ~docv:"FILE"

let setup_mails_from_in_channel = function
  | None -> []
  | Some None -> setup_mails_from_in_channel stdin
  | Some (Some filename) ->
      let ic = open_in (Fpath.to_string filename) in
      let finally () = close_in ic in
      Fun.protect ~finally @@ fun () -> setup_mails_from_in_channel ic

let setup_mails_from_in_channel =
  let open Term in
  const setup_mails_from_in_channel $ list_of_mails

let output =
  let doc = "The output file where to save the PACK file." in
  let parser str =
    match Fpath.of_string str with
    | Ok value ->
        if Sys.file_exists str
        then error_msgf "%a already exists" Fpath.pp value
        else Ok value
    | Error _ as err -> err in
  let non_existing_file = Arg.conv (parser, Fpath.pp) in
  let open Arg in
  value
  & opt (some non_existing_file) None
  & info [ "o"; "output" ] ~doc ~docv:"FILE"

let pack =
  let doc = "The PACKv2 file." in
  let open Arg in
  required & pos 0 (some existing_file) None & info [] ~doc ~docv:"PACK"

let default_threads = Int.min 4 (Stdlib.Domain.recommended_domain_count () - 1)

let threads =
  let doc = "The number of threads to allocate for the PACKv2 verification." in
  let open Arg in
  value & opt int default_threads & info [ "t"; "threads" ] ~doc ~docv:"NUMBER"

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
  required & pos 2 (some identifier) None & info [] ~doc ~docv:"IDENTIFIER"

let make_term =
  let open Term in
  let to_ret = function
    | Ok () -> `Ok ()
    | Error exn -> `Error (false, Printexc.to_string exn) in
  ret
    (app (const to_ret)
       (const run_make
       $ setup_logs
       $ setup_mails_from_in_channel
       $ mails
       $ output))

let list_term =
  let open Term in
  const run_list $ setup_logs $ pack

let index_term =
  let open Term in
  const run_index $ setup_logs $ threads $ pack

let get_term =
  let pack =
    let doc = "The PACKv2 file." in
    let open Arg in
    value & pos 0 (some existing_file) None & info [] ~doc ~docv:"PACK" in
  let idx =
    let doc = "The IDX file." in
    let open Arg in
    required & pos 1 (some existing_file) None & info [] ~doc ~docv:"IDX" in
  let open Term in
  const run_get $ setup_logs $ pack $ idx $ identifier

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

let default = Term.(ret (const (`Help (`Pager, None))))

let () =
  let doc = "A tool to manipulate PACKv2 which contains emails." in
  let man = [] in
  let cmd =
    Cmd.group ~default
      (Cmd.info "pack" ~doc ~man)
      [ make_cmd; list_cmd; index_cmd; get_cmd ] in
  Cmd.(exit @@ eval cmd)
