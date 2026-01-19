let src = Logs.Src.create "blaze.pack"

module Log = (val Logs.src_log src : Logs.LOG)

let ( % ) f g = fun x -> f (g x)

let pp_kind ppf = function
  | `A -> Fmt.string ppf "mail"
  | `B -> Fmt.string ppf "blob"
  | `C -> Fmt.string ppf "stem"
  | `D -> Fmt.string ppf "tree"

let mail_identify =
  let open Digestif in
  let init kind (len : Carton.Size.t) =
    let hdr = Fmt.str "%a %d\000" pp_kind kind (len :> int) in
    let ctx = SHA1.empty in
    SHA1.feed_string ctx hdr in
  let feed bstr ctx = SHA1.feed_bigstring ctx bstr in
  let serialize = SHA1.(Carton.Uid.unsafe_of_string % to_raw_string % get) in
  { Carton.First_pass.init; feed; serialize }

let uid_of_value value =
  let k = Carton.Value.kind value in
  let bstr = Carton.Value.bigstring value in
  let len = Carton.Value.length value in
  let open Carton.First_pass in
  let ctx = mail_identify.init k (Carton.Size.of_int_exn len) in
  let ctx = mail_identify.feed (Bigarray.Array1.sub bstr 0 len) ctx in
  mail_identify.Carton.First_pass.serialize ctx

let filepath_to_email filepath =
  match Email.of_filepath filepath with
  | Ok (t, metadata) -> (filepath, t, metadata)
  | Error `Invalid_email ->
      Log.err (fun m -> m "%a is an invalid email" Fpath.pp filepath) ;
      Fmt.failwith "Invalid email"
  | Error `No_symmetry ->
      Log.err (fun m ->
          m "%a has no symmetry between skeleton and Mr.MIME semantic layout"
            Fpath.pp filepath) ;
      Fmt.failwith "Invalid email: no symmetry"
  | Error `Not_enough ->
      Log.err (fun m -> m "%a is probably truncated" Fpath.pp filepath) ;
      Fmt.failwith "Invalid email: truncated email"
  | Error (`Msg msg) ->
      Log.err (fun m -> m "%a: %s" Fpath.pp filepath msg) ;
      Fmt.failwith "Invalid email: %s" msg

type src =
  | Mail of string
  | Body of Fpath.t * int * int
  | Stem of Carton.Uid.t * Carton.Uid.t * int * (string, int) Hashtbl.t
  | Tree of string

let none_if_stop lang =
  match List.assoc_opt lang Stopwords.words with
  | Some stops -> fun stem -> if List.mem stem stops then None else Some stem
  | None -> Option.some

let actions = Tokenizer.[ (Whitespace, Remove); (Bert, Remove) ]

let freqs_of_document fd ?(off = 0) ?len lang =
  let len =
    match len with
    | Some len -> len
    | None -> (Unix.fstat fd).Unix.st_size - off in
  let barr =
    Unix.map_file fd ~pos:(Int64.of_int off) Bigarray.char Bigarray.c_layout
      false [| len |] in
  let bstr = Bigarray.array1_of_genarray barr in
  let words = Tokenizer.run_on_bstr actions bstr in
  let tbl = Hashtbl.create 0x7ff in
  let stemmer = Snowball.create ~encoding:UTF_8 lang in
  let finally () = Snowball.remove stemmer in
  Fun.protect ~finally @@ fun () ->
  let fn word =
    let stem = try Some (Snowball.stem stemmer word) with _ -> None in
    let stem = Option.bind stem (none_if_stop lang) in
    let count = Option.bind stem (Hashtbl.find_opt tbl) in
    match (stem, count) with
    | None, _ -> ()
    | Some stem, None -> Hashtbl.add tbl stem 1
    | Some stem, Some count -> Hashtbl.replace tbl stem (count + 1) in
  Seq.iter fn words ;
  let length = Hashtbl.length tbl in
  (length, tbl)

let email_to_entries filepath t =
  let open Cartonnage in
  let fd = Unix.openfile (Fpath.to_string filepath) Unix.[ O_RDONLY ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let fn (pos, pos_end) =
    let len = pos_end - pos in
    let barr =
      Unix.map_file fd ~pos:(Int64.of_int pos) Bigarray.char Bigarray.c_layout
        false [| len |] in
    let bstr = Bigarray.array1_of_genarray barr in
    let ctx =
      mail_identify.Carton.First_pass.init `B (Carton.Size.of_int_exn len) in
    let ctx = mail_identify.Carton.First_pass.feed bstr ctx in
    let uid = mail_identify.Carton.First_pass.serialize ctx in
    (pos, len, uid) in
  let t = Email.map fn t in
  let fn0 entries (pos, len, hash) =
    let kind = `B in
    let entry = Entry.make ~kind ~length:len hash (Body (filepath, pos, len)) in
    entry :: entries in
  (* NOTE(dinosaure): our [skeleton] has everything and our [semantic] has few
     documents which must be available into our [skeleton]. To collect all
     parts, we prefer to [fold] on our [skeleton]. *)
  let entries0 = Email.Skeleton.fold fn0 [] (fst t) in
  let fn1 (_, _, (hash : Carton.Uid.t)) = (hash :> string) in
  let serialized = Email.to_string (Email.map fn1 t) in
  let hash =
    let hdr = Fmt.str "mail %d\000" (String.length serialized) in
    Digestif.SHA1.digest_string (hdr ^ serialized) in
  let hash = Digestif.SHA1.to_raw_string hash in
  let hash = Carton.Uid.unsafe_of_string hash in
  let entry =
    Entry.make ~kind:`A ~length:(String.length serialized) hash
      (Mail serialized) in
  let fn2 (mail : Carton.Uid.t) entries
      (_mime, lang, (off, len, (blob : Carton.Uid.t))) =
    let tbl_len, tbl = freqs_of_document fd ~off ~len lang in
    let length, hash =
      Stem.to_length_and_hash ((mail :> string), (blob :> string), tbl_len, tbl)
    in
    let hash = Carton.Uid.unsafe_of_string hash in
    let kind = `C in
    let value = Stem (mail, blob, tbl_len, tbl) in
    let entry = Entry.make ~kind ~length hash value in
    entry :: entries in
  let entries1 = Email.Semantic.fold (fn2 hash) [] (snd t) in
  entry :: List.rev_append entries1 entries0

let entry_of_tree t =
  let emitter = Encore.to_lavoisier Tree.Format.t in
  let serialized = Encore.Lavoisier.emit_string ~chunk:0x7ff t emitter in
  let hash =
    let hdr = Fmt.str "mail %d\000" (String.length serialized) in
    let ctx = Digestif.SHA1.empty in
    let ctx = Digestif.SHA1.feed_string ctx hdr in
    let ctx = Digestif.SHA1.feed_string ctx serialized in
    Digestif.SHA1.get ctx in
  let hash = Digestif.SHA1.to_raw_string hash in
  let hash = Carton.Uid.unsafe_of_string hash in
  Cartonnage.Entry.make ~kind:`D ~length:(String.length serialized) hash
    (Tree serialized)

let sha1_with_ctx (ctx : Digestif.SHA1.ctx) =
  let module Hash = Digestif.SHA1 in
  let feed_bigstring bstr ctx = Hash.feed_bigstring ctx bstr in
  let feed_bytes buf ~off ~len ctx = Hash.feed_bytes ctx ~off ~len buf in
  let hash =
    {
      Carton.First_pass.feed_bytes;
      feed_bigstring;
      serialize = Hash.to_raw_string % Hash.get;
      length = Hash.digest_size;
    } in
  Carton.First_pass.Digest (hash, ctx)

let sha1 = sha1_with_ctx Digestif.SHA1.empty

let config ?pagesize ?cachesize ?threads ?on_entry ?on_object () =
  let ref_length = Digestif.SHA1.digest_size in
  Carton_miou_unix.config ?pagesize ?cachesize ?threads ?on_entry ?on_object
    ~ref_length (Carton.Identify mail_identify)

let delta ~load entries =
  let ref_length = Digestif.SHA1.digest_size in
  Carton_miou_unix.delta ~ref_length ~load entries

let to_pack ?with_header ?with_signature ~load targets =
  let with_signature =
    match with_signature with
    | Some ctx -> Some (sha1_with_ctx ctx)
    | None -> None in
  Carton_miou_unix.to_pack ?with_header ?with_signature ~load targets

let make ?index filename =
  let ref_length = Digestif.SHA1.digest_size in
  Carton_miou_unix.make ~ref_length ?index filename

let index filename =
  let ref_length = Digestif.SHA1.digest_size in
  let hash_length = ref_length in
  Carton_miou_unix.index ~hash_length ~ref_length filename

let verify_from_pack ~cfg filename =
  Carton_miou_unix.verify_from_pack ~cfg ~digest:sha1 filename

let verify_from_idx ~cfg filename =
  Carton_miou_unix.verify_from_idx ~cfg ~digest:sha1 filename

let list ?(kind = `A) filename =
  let ref_length = Digestif.SHA1.digest_size in
  let zero = Carton.Size.zero in
  let pack = make filename in
  let objects_by_offsets = Hashtbl.create 0x7ff in
  let objects_by_refs = Hashtbl.create 0x7ff in
  let is_a_object ?offset ?ref () =
    match (offset, ref) with
    | None, None -> false
    | Some offset, _ -> Hashtbl.mem objects_by_offsets offset
    | None, Some ref -> Hashtbl.mem objects_by_refs ref in
  let fn = function
    | `Number _ | `Hash _ -> None
    | `Entry { Carton.First_pass.kind = Base kind'; offset; size; _ }
      when kind = kind' ->
        Hashtbl.add objects_by_offsets offset () ;
        let blob = Carton.Blob.make ~size in
        let value = Carton.of_offset pack blob ~cursor:offset in
        let uid = uid_of_value value in
        Hashtbl.add objects_by_refs uid () ;
        Some (offset, value)
    | `Entry { Carton.First_pass.kind = Base _; _ } -> None
    | `Entry { Carton.First_pass.kind = Ofs { sub; _ }; offset; _ } ->
        let parent = offset - sub in
        if is_a_object ~offset:parent ()
        then begin
          Hashtbl.add objects_by_offsets offset () ;
          let size = Carton.size_of_offset pack ~cursor:offset zero in
          let blob = Carton.Blob.make ~size in
          let value = Carton.of_offset pack blob ~cursor:offset in
          let uid = uid_of_value value in
          Hashtbl.add objects_by_refs uid () ;
          Some (offset, value)
        end
        else None
    | `Entry { Carton.First_pass.kind = Ref { ptr; _ }; offset; _ } ->
        (* TODO(dinosaure): we don't handle a cascade of [OBJ_REF]! Our [pack]
           was not made with an [index]. *)
        if is_a_object ~ref:ptr ()
        then begin
          Hashtbl.add objects_by_offsets offset () ;
          let size = Carton.size_of_offset pack ~cursor:offset zero in
          let blob = Carton.Blob.make ~size in
          let value = Carton.of_offset pack blob ~cursor:offset in
          let uid = uid_of_value value in
          Hashtbl.add objects_by_refs uid () ;
          Some (offset, value)
        end
        else None
    | _ -> None in
  let open Flux.Flow in
  Carton_miou_flux.first_pass ~digest:sha1 ~ref_length << filter_map fn
