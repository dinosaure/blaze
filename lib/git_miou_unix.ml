let src = Logs.Src.create "blaze.git-miou-unix"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let@ ) finally fn = Fun.protect ~finally fn
let ( let* ) = Result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let inhibit fn = try fn () with _exn -> ()

type error = [ Smart.error | `Msg of string ]

let pp_error ppf = function
  | #Smart.error as err -> Smart.pp_error ppf err
  | `Msg msg -> Fmt.string ppf msg

let rec clear fd = function
  | Protocol.Error err -> Result.Error err
  | Return v -> Ok v
  | Read { k; buffer; off; len } ->
      let len = Miou_unix.read fd ~off ~len buffer in
      let res = if len = 0 then `End else `Len len in
      clear fd (k res)
  | Write { k; buffer; off; len } ->
      Miou_unix.write fd ~off ~len buffer ;
      clear fd (k len)

let rec through (ic, oc) = function
  | Protocol.Error err -> Result.Error err
  | Return v -> Ok v
  | Read { k; buffer; off; len } ->
      let len = Stdlib.input ic buffer off len in
      Log.debug (fun m -> m "+%d byte(s)" len) ;
      Log.debug (fun m ->
          m "@[<hov>%a@]"
            (Hxd_string.pp Hxd.default)
            (Bytes.sub_string buffer off len)) ;
      let res = if len = 0 then `End else `Len len in
      through (ic, oc) (k res)
  | Write { k; buffer; off; len } ->
      output_substring oc buffer off len ;
      flush oc ;
      through (ic, oc) (k len)

let digest =
  let open Digestif in
  let feed_bigstring bstr ctx = SHA1.feed_bigstring ctx bstr in
  let feed_bytes buf ~off ~len ctx = SHA1.feed_bytes ctx ~off ~len buf in
  let hash =
    {
      Carton.First_pass.feed_bytes;
      feed_bigstring;
      serialize = Fun.compose SHA1.to_raw_string SHA1.get;
      length = SHA1.digest_size;
    } in
  Carton.First_pass.Digest (hash, SHA1.empty)

let identify =
  let open Digestif in
  let pp_kind ppf = function
    | `A -> Fmt.string ppf "commit"
    | `B -> Fmt.string ppf "tree"
    | `C -> Fmt.string ppf "blob"
    | `D -> Fmt.string ppf "tag" in
  let init kind (len : Carton.Size.t) =
    let hdr = Fmt.str "%a %d\000" pp_kind kind (len :> int) in
    let ctx = SHA1.empty in
    SHA1.feed_string ctx hdr in
  let feed bstr ctx = SHA1.feed_bigstring ctx bstr in
  let ( $ ) = Fun.compose in
  let serialize = SHA1.(Carton.Uid.unsafe_of_string $ to_raw_string $ get) in
  { Carton.First_pass.init; feed; serialize }

let fetch_over_ssh ~user ~server ?(port = 22) path =
  let remote = Fmt.str "%s@%s" user server in
  let cmd = Fmt.str "git-upload-pack '%s'" path in
  let cmd =
    Fmt.str "ssh -p %d %s GIT_PROTOCOL=version=2 %a" port remote
      Fmt.(quote string)
      cmd in
  let ctx = Protocol.ctx () in
  let from =
    Flux.Source.with_task ~size:0x7ff @@ fun q ->
    let resource = Miou.Ownership.create ~finally:Flux.Bqueue.close q in
    Miou.Ownership.own resource ;
    let@ () = fun () -> Miou.Ownership.release resource in
    let t = Smart.clone ~protocol:`SSH ctx q in
    let ic, oc = Unix.open_process cmd in
    let git_result = through (ic, oc) t in
    let cmd_result = Unix.close_process (ic, oc) in
    match (git_result, cmd_result) with
    | Ok false, Unix.WEXITED 0 -> ()
    | Ok _, Unix.WEXITED n ->
        Log.err (fun m -> m "SSH process exited with %d code" n)
    | _, Unix.(WSIGNALED _ | WSTOPPED _) ->
        Log.err (fun m -> m "SSH process abnormally stopped")
    | Error err, _ -> Log.err (fun m -> m "%a" Smart.pp_error err) in
  Ok from

let fetch_over_tcp ~server ?(port = 9418) path he =
  let* _, socket = Happy_eyeballs_miou_unix.connect he server [ port ] in
  let ctx = Protocol.ctx () in
  let from =
    Flux.Source.with_task ~size:0x7ff @@ fun q ->
    let t = Smart.clone ~protocol:(`Git path) ctx q in
    let@ () = fun () -> Miou_unix.close socket in
    match clear socket t with
    | Ok false -> ()
    | Ok true -> Log.warn (fun m -> m "Remote Git server failed")
    | Error err -> Log.err (fun m -> m "%a" Smart.pp_error err) in
  Ok from

let rec collect buf = function
  | Protocol.Write { k; buffer; off; len } ->
      Buffer.add_substring buf buffer off len ;
      collect buf (k len)
  | state -> state

let rec unroll (q, rem) = function
  | Protocol.Error err -> Result.Error (err :> error)
  | Return v -> Ok v
  | Write _ -> Error (`Msg "Unexpected write from the Smart protocol over HTTP")
  | Read { k; buffer; off; len } -> begin
      let rec next = function
        | "" -> Option.bind (Flux.Bqueue.get q) next
        | str -> Some str in
      match next rem with
      | None -> unroll (q, "") (k `End)
      | Some str ->
          let len = Int.min len (String.length str) in
          Bytes.blit_string str 0 buffer off len ;
          let rem = String.sub str len (String.length str - len) in
          unroll (q, rem) (k (`Len len))
    end

let is_redirection (resp : Httpcats.response) =
  H2.Status.is_redirection resp.Httpcats.status

let request ~resolver ?authenticator ?(meth = `GET) ?(headers = []) ?body uri
    state =
  let q = Flux.Bqueue.(create with_close_and_halt) 0x7ff in
  let push str = inhibit @@ fun () -> Flux.Bqueue.put q str in
  let fn _meta _req resp () = function
    | Some str when not (is_redirection resp) -> push str
    | _ -> () in
  let prm =
    Miou.async @@ fun () ->
    let@ () = fun () -> inhibit @@ fun () -> Flux.Bqueue.close q in
    let body = Option.map (fun str -> Httpcats.String str) body in
    Httpcats.request ~resolver ?authenticator ~follow_redirect:true ~meth
      ~headers ?body ~fn ~uri () in
  let result = unroll (q, "") state in
  Flux.Bqueue.halt q ;
  let status = Miou.await prm in
  match (result, status) with
  | result, Ok (Ok (resp, ())) when resp.Httpcats.status = `OK -> result
  | _, Ok (Ok (resp, ())) ->
      error_msgf "%s: unexpected response %a" uri H2.Status.pp_hum
        resp.Httpcats.status
  | _, Ok (Error err) -> error_msgf "%s: %a" uri Httpcats.pp_error err
  | _, Error exn -> error_msgf "%s: %s" uri (Printexc.to_string exn)

let user_agent = ("User-Agent", "git/blaze")

let advertise_over_http ~resolver ?authenticator uri =
  let headers = [ ("Git-Protocol", "version=2"); user_agent ] in
  let ctx = Protocol.ctx () in
  request ~resolver ?authenticator ~headers
    (uri ^ "/info/refs?service=git-upload-pack")
    (Smart.advertisement ctx)

let post_over_http ~resolver ?authenticator uri state =
  let headers =
    [
      ("Content-Type", "application/x-git-upload-pack-request");
      ("Accept", "application/x-git-upload-pack-result");
      ("Git-Protocol", "version=2");
      user_agent;
    ] in
  let buf = Buffer.create 0x7ff in
  let state = collect buf state in
  let body = Buffer.contents buf in
  request ~resolver ?authenticator ~meth:`POST ~headers ~body
    (uri ^ "/git-upload-pack") state

let fetch_over_http ?authenticator uri he =
  let resolver = `Happy he in
  let uri =
    if String.ends_with ~suffix:"/" uri
    then String.sub uri 0 (String.length uri - 1)
    else uri in
  let from =
    Flux.Source.with_task ~size:0x7ff @@ fun q ->
    let resource = Miou.Ownership.create ~finally:Flux.Bqueue.close q in
    Miou.Ownership.own resource ;
    let@ () = fun () -> Miou.Ownership.release resource in
    let result =
      let* advertisement = advertise_over_http ~resolver ?authenticator uri in
      match advertisement with
      | Smart.V1 { refs; capabilities } ->
          let ctx = Protocol.ctx () in
          post_over_http ~resolver ?authenticator uri
            (Smart.fetch_v1 ~capabilities ~want:refs.head q ctx)
      | Smart.V2 _ ->
          let* refs =
            post_over_http ~resolver ?authenticator uri
              (Smart.ls_refs (Protocol.ctx ())) in
          let ctx = Protocol.ctx () in
          post_over_http ~resolver ?authenticator uri
            (Smart.fetch_v2 ~want:refs.head q ctx) in
    match result with
    | Ok false -> ()
    | Ok true -> Log.warn (fun m -> m "Remote Git server failed")
    | Error err -> Log.err (fun m -> m "%a" pp_error err) in
  Ok from

let fetch ?authenticator remote he producer =
  let@ () = fun () -> Flux.Bqueue.close producer in
  let* from =
    match remote with
    | `Git (server, port, path) -> fetch_over_tcp ~server ?port path he
    | `SSH (user, server, port, path) -> fetch_over_ssh ~user ~server ?port path
    | `HTTP uri -> fetch_over_http ?authenticator uri he in
  let filename = Filename.temp_file "public-inbox-" ".pack" in
  let oc = open_out_bin filename in
  let@ () = fun () -> close_out oc in
  let into_filename str =
    output_string oc str ;
    flush oc in
  let via =
    let open Flux.Flow in
    let flow = Carton_miou_flux.first_pass ~ref_length:20 ~digest in
    compose (tap into_filename) flow in
  let into = Carton_miou_flux.oracle ~identify in
  Log.debug (fun m -> m "Start to analyze incoming PACK file") ;
  let oracle, _leftover = Flux.Stream.run ~from ~via ~into in
  Log.debug (fun m -> m "Repository cloned into %s" filename) ;
  let pack = Carton_miou_unix.make ~ref_length:20 (Fpath.v filename) in
  let entries = Carton_miou_flux.entries pack oracle in
  let fn (value, _cursor, uid) =
    match Carton.Value.kind value with
    | `C ->
        Log.debug (fun m -> m "New Git object: %a" Carton.Uid.pp uid) ;
        let raw = Carton.Value.bigstring value in
        Log.debug (fun m ->
            m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) (Bstr.to_string raw)) ;
        Flux.Bqueue.put producer (uid, raw)
    | _ -> () in
  Log.debug (fun m -> m "Start to collect Git objects") ;
  Flux.Source.each fn entries ;
  Ok ()
