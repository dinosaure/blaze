let src = Logs.Src.create "smart"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Protocol.bind

type error =
  [ Protocol.error
  | `No_branch
  | `Invalid_version of string
  | `Invalid_negotiation
  | `No_side_band
  | `Err of string ]

let pp_error ppf = function
  | #Protocol.error as err -> Protocol.pp_error ppf err
  | `No_branch -> Fmt.string ppf "No branch available"
  | `Invalid_version v -> Fmt.pf ppf "Invalid Smart version: %S" v
  | `Invalid_negotiation -> Fmt.string ppf "Failed to negotiate"
  | `No_side_band ->
      Fmt.string ppf "The remote does not support the side-band capability"
  | `Err msg -> Fmt.pf ppf "Remote error: %s" msg

type refs = {
  refs : (string * string) list (* refname, oid (hex) *);
  head : string; (* oid (hex) *)
  head_symref : string option (* refs/heads/main *);
}

type advertisement =
  | V1 of { refs : refs; capabilities : string list }
  | V2 of { capabilities : string list }

let attribute ~prefix attrs =
  let fn attr =
    if String.starts_with ~prefix attr
    then
      let off = String.length prefix
      and len = String.length attr - String.length prefix in
      Some (String.sub attr off len)
    else None in
  List.find_map fn attrs

let err_of_pkt pkt = attribute ~prefix:"ERR " [ pkt ]

let head_of_refs refs head_symref =
  match (List.assoc_opt "HEAD" refs, head_symref) with
  | Some head, _ when head <> "unborn" -> Protocol.return head
  | _, Some symref ->
      begin match List.assoc_opt symref refs with
      | Some head -> Protocol.return head
      | None -> Protocol.error `No_branch
      end
  | _ -> Protocol.error `No_branch

let ref_of_line line =
  match String.split_on_char ' ' (String.trim line) with
  | [ oid; name ] -> Some (name, oid)
  | _ -> None

let advertisement_v1 first ctx =
  let first, capabilities =
    match String.index_opt first '\000' with
    | Some idx ->
        ( String.sub first 0 idx,
          String.sub first (idx + 1) (String.length first - idx - 1) )
    | None -> (first, "") in
  let capabilities =
    List.filter (( <> ) "")
      (String.split_on_char ' ' (String.trim capabilities)) in
  let head_symref = attribute ~prefix:"symref=HEAD:" capabilities in
  let rec go acc ctx =
    let* pkt = Protocol.decode_pkt ctx in
    match String.trim pkt with
    | "" -> Protocol.return (List.rev acc)
    | line ->
        begin match ref_of_line line with
        | Some value -> go (value :: acc) ctx
        | None -> Protocol.error `Invalid_pkt_line
        end in
  match (err_of_pkt first, ref_of_line first) with
  | Some msg, _ -> Protocol.error (`Err msg)
  | None, None -> Protocol.error `Invalid_pkt_line
  | None, Some value ->
      let* refs = go [ value ] ctx in
      let fn (name, _) =
        name <> "capabilities^{}" && not (String.ends_with ~suffix:"^{}" name)
      in
      let refs = List.filter fn refs in
      let* head = head_of_refs refs head_symref in
      Protocol.return (V1 { refs = { refs; head; head_symref }; capabilities })

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
      let* capabilities = capabilities [] ctx in
      Protocol.return (V2 { capabilities })
  | [ "version"; ("0" | "1") ] ->
      let* pkt = Protocol.decode_pkt ctx in
      advertisement_v1 (String.trim pkt) ctx
  | [ "version"; v ] -> Protocol.error (`Invalid_version v)
  | _ -> advertisement_v1 pkt ctx

(* NOTE(dinosaure): we need to split [ls_refs] and [fetch] for HTTP. *)

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
                match attribute ~prefix:"symref-target:" attrs with
                | Some _ as value -> value
                | None -> head_symref
              else head_symref in
            go ((name, oid) :: acc) head_symref ctx
        | _ -> Protocol.error `Invalid_pkt_line
        end in
  let* refs, head_symref = go [] None ctx in
  let* head = head_of_refs refs head_symref in
  Protocol.return { refs; head; head_symref }

let rec side_band errored q ctx =
  let* pkt = Protocol.decode_pkt ctx in
  if String.length pkt = 0
  then Protocol.return errored
  else
    let data = String.sub pkt 1 (String.length pkt - 1) in
    match pkt.[0] with
    | '\001' ->
        Flux.Bqueue.put q data ;
        side_band errored q ctx
    | '\003' ->
        Log.err (fun m -> m "[remote]: %s" data) ;
        side_band true q ctx
    | _ -> side_band errored q ctx

let fetch_v2 ~want q ctx =
  let* () = Protocol.encode_pkt ctx "command=fetch" in
  let* () = Protocol.encode_pkt ctx "object-format=sha1" in
  let* () = Protocol.encode_delim_pkt ctx in
  let* () = Protocol.encode_pkt ctx "ofs-delta" in
  let* () = Protocol.encode_pkt ctx "no-progress" in
  let* () = Protocol.encode_pkt ctx "want %s" want in
  let* () = Protocol.encode_pkt ctx "done" in
  let* () = Protocol.encode_flush_pkt ctx in
  (* NOTE(dinosaure): no negotiation *)
  let* () =
    let* pkt = Protocol.decode_pkt ctx in
    match String.trim pkt with
    | "packfile" -> Protocol.return ()
    | pkt ->
        Log.err (fun m -> m "Unexpected section: %S" pkt) ;
        Protocol.error `Invalid_pkt_line in
  side_band false q ctx

let capabilities_v1 capabilities =
  let side_band =
    if List.mem "side-band-64k" capabilities
    then Some "side-band-64k"
    else if List.mem "side-band" capabilities
    then Some "side-band"
    else None in
  let advertised capability = List.mem capability capabilities in
  let fn side_band =
    side_band :: List.filter advertised [ "ofs-delta"; "no-progress" ] in
  Option.map fn side_band

let fetch_v1 ~capabilities ~want q ctx =
  match capabilities_v1 capabilities with
  | None -> Protocol.error `No_side_band
  | Some caps ->
      let* () =
        Protocol.encode_pkt ctx "want %s %s\n" want (String.concat " " caps)
      in
      let* () = Protocol.encode_flush_pkt ctx in
      let* () = Protocol.encode_pkt ctx "done\n" in
      let* () =
        let* pkt = Protocol.decode_pkt ctx in
        match String.trim pkt with
        | "NAK" -> Protocol.return ()
        | pkt ->
            begin match err_of_pkt pkt with
            | Some msg -> Protocol.error (`Err msg)
            | None ->
                Log.err (fun m -> m "Unexpected acknowledgement: %S" pkt) ;
                Protocol.error `Invalid_pkt_line
            end in
      side_band false q ctx

let clone ~protocol ctx q =
  let* () =
    match protocol with
    | `Git path ->
        Protocol.encode_pkt ctx
          "git-upload-pack %s\000host=localhost\000\000version=2\000" path
    | _ -> Protocol.return () in
  let* advertisement = advertisement ctx in
  match advertisement with
  | V1 { refs; capabilities } -> fetch_v1 ~capabilities ~want:refs.head q ctx
  | V2 _ ->
      let* refs = ls_refs ctx in
      fetch_v2 ~want:refs.head q ctx
