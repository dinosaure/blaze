let src = Logs.Src.create "smart"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Protocol.bind

let rec comment ctx =
  let* pkt = Protocol.decode_pkt ctx in
  if String.length pkt > 0 then comment ctx else Protocol.return ()

let _zero_uid = String.for_all (function '0' -> true | _ -> false)

let clonev2 ctx q =
  let* pkt = Protocol.decode_pkt ctx in
  let* () =
    if String.length pkt >= 1 && pkt.[0] = '#'
    then comment ctx
    else Protocol.return () in
  match String.split_on_char ' ' (String.trim pkt) with
  | [ "version"; "2" ] ->
      let* _capabilities =
        let rec go acc ctx =
          let* pkt = Protocol.decode_pkt ctx in
          match String.trim pkt with
          | "" -> Protocol.return (List.rev acc)
          | capability -> go (capability :: acc) ctx in
        go [] ctx in
      let* () = Protocol.encode_pkt ctx "command=ls-refs\n" in
      let* () = Protocol.encode_pkt ctx "object-format=sha1" in
      let* () = Protocol.encode_delim_pkt ctx in
      let* () = Protocol.encode_flush_pkt ctx in
      let* refs =
        let rec go acc ctx =
          let* pkt = Protocol.decode_pkt ctx in
          match String.trim pkt with
          | "" -> Protocol.return (List.rev acc)
          | str ->
              let[@warning "-8"] (hash :: reference) =
                String.split_on_char ' ' str in
              let reference = String.concat " " reference in
              go ((reference, hash) :: acc) ctx in
        go [] ctx in
      let* head =
        match List.assoc "HEAD" refs with
        | hash -> Protocol.return hash
        | exception Not_found -> Protocol.error `No_branch in
      let* () = Protocol.encode_pkt ctx "command=fetch" in
      let* () = Protocol.encode_pkt ctx "object-format=sha1" in
      let* () = Protocol.encode_delim_pkt ctx in
      let* () = Protocol.encode_pkt ctx "ofs-delta" in
      let* () = Protocol.encode_pkt ctx "no-progress" in
      let* () = Protocol.encode_pkt ctx "want %s" head in
      let* () = Protocol.encode_pkt ctx "done" in
      let* () = Protocol.encode_flush_pkt ctx in
      let* () =
        let* pkt = Protocol.decode_pkt ctx in
        match String.trim pkt with
        | "packfile" -> Protocol.return ()
        | _ -> Protocol.error `Invalid_pkt_line in
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
  | [ "version"; v ] -> Protocol.error (`Invalid_version v)
  | _ -> Protocol.error `Invalid_pkt_line

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
  clonev2 ctx q
