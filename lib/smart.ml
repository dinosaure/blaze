let src = Logs.Src.create "smart"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Protocol.bind

type refs = {
  refs : (string * string) list (* refname, oid (hex) *);
  head : string; (* oid (hex) *)
  head_symref : string option (* refs/heads/main *);
}

let advertisement ctx =
  let rec version ctx =
    let* pkt = Protocol.decode_pkt ctx in
    match String.trim pkt with
    | "" -> version ctx
    | pkt when pkt.[0] = '#' -> version ctx (* NOTE(dinosaure): for HTTP *)
    | pkt -> Protocol.return pkt in
  let* pkt = version ctx in
  match String.split_on_char ' ' pkt with
  | [ "version"; "2" ] ->
      let rec capabilities acc ctx =
        let* pkt = Protocol.decode_pkt ctx in
        match String.trim pkt with
        | "" -> Protocol.return (List.rev acc)
        | capability -> capabilities (capability :: acc) ctx in
      capabilities [] ctx
  | [ "version"; v ] -> Protocol.error (`Invalid_version v)
  | _ -> Protocol.error `Invalid_pkt_line

let ls_refs ctx =
  let* () = Protocol.encode_pkt ctx "command=ls-refs\n" in
  let* () = Protocol.encode_pkt ctx "object-format=sha1" in
  let* () = Protocol.encode_delim_pkt ctx in
  let* () = Protocol.encode_pkt ctx "symrefs" in
  (* NOTE(dinosaure): filter references. *)
  let* () = Protocol.encode_pkt ctx "ref-prefix HEAD" in
  let* () = Protocol.encode_pkt ctx "ref-prefix refs/heads/" in
  let* () = Protocol.encode_pkt ctx "ref-prefix refs/tags/" in
  let* () = Protocol.encode_flush_pkt ctx in
  let symref_target attrs =
    let prefix = "symref-target:" in
    let fn attr =
      if String.starts_with ~prefix attr
      then
        let off = String.length prefix
        and len = String.length attr - String.length prefix in
        let attr = String.sub attr off len in
        Some attr
      else None in
    List.find_map fn attrs in
  let rec go acc head_symref ctx =
    let* pkt = Protocol.decode_pkt ctx in
    match String.trim pkt with
    | "" -> Protocol.return (List.rev acc, head_symref)
    | line ->
        begin match String.split_on_char ' ' line with
        | oid :: name :: attrs ->
            let head_symref =
              if name = "HEAD"
              then
                match symref_target attrs with
                | Some _ as value -> value
                | None -> head_symref
              else head_symref in
            go ((name, oid) :: acc) head_symref ctx
        | _ -> Protocol.error `Invalid_pkt_line
        end in
  let* refs, head_symref = go [] None ctx in
  let* head =
    match (List.assoc_opt "HEAD" refs, head_symref) with
    | Some head, _ when head <> "unborn" -> Protocol.return head
    | _, Some symref ->
        begin match List.assoc_opt symref refs with
        | Some head -> Protocol.return head
        | None -> Protocol.error `No_branch
        end
    | _ -> Protocol.error `No_branch in
  Protocol.return { refs; head; head_symref }

let fetch ~want q ctx =
  let* () = Protocol.encode_pkt ctx "command=fetch" in
  let* () = Protocol.encode_pkt ctx "object-format=sha1" in
  let* () = Protocol.encode_delim_pkt ctx in
  let* () = Protocol.encode_pkt ctx "ofs-delta" in
  let* () = Protocol.encode_pkt ctx "no-progress" in
  let* () = Protocol.encode_pkt ctx "want %s" want in
  let* () = Protocol.encode_pkt ctx "done" in
  (* NOTE(dinosaure): no negotiation *)
  let* () =
    let* pkt = Protocol.decode_pkt ctx in
    match String.trim pkt with
    | "packfile" -> Protocol.return ()
    | pkt ->
        Log.err (fun m -> m "Unexpected section: %S" pkt) ;
        Protocol.error `Invalid_pkt_line in
  let rec go errored ctx =
    let* pkt = Protocol.decode_pkt ctx in
    if String.length pkt = 0
    then Protocol.return errored
    else
      let data = String.sub pkt 1 (String.length pkt - 1) in
      match pkt.[0] with
      | '\001' ->
          Flux.Bqueue.put q data ;
          go errored ctx
      | '\003' ->
          Log.err (fun m -> m "[remote]: %s" data) ;
          go true ctx
      | _ -> go errored ctx in
  go false ctx

type error =
  [ Protocol.error
  | `No_branch
  | `Invalid_version of string
  | `Invalid_negotiation ]

let pp_error ppf = function
  | #Protocol.error as err -> Protocol.pp_error ppf err
  | `No_branch -> Fmt.string ppf "No branch available"
  | `Invalid_version v -> Fmt.pf ppf "Invalid Smart version: %S" v
  | `Invalid_negotiation -> Fmt.string ppf "Failed to negotiate"

let clone ~protocol ctx q =
  let* () =
    match protocol with
    | `Git path ->
        Protocol.encode_pkt ctx
          "git-upload-pack %s\000host=localhost\000\000version=2\000" path
    | _ -> Protocol.return () in
  let* _capabilities = advertisement ctx in
  let* refs = ls_refs ctx in
  let want = refs.head in
  let* errored = fetch ~want q ctx in
  Protocol.return errored
