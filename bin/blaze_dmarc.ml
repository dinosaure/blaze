let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let stamp newline domain fpath value output =
  let oc, oc_close =
    match output with
    | None -> (stdout, ignore)
    | Some fpath ->
        let oc = open_out (Fpath.to_string fpath) in
        (oc, close_out) in
  let finally () = oc_close oc in
  Fun.protect ~finally @@ fun () ->
  let value =
    let new_line = match newline with `CRLF -> "\r\n" | `LF -> "\n" in
    Prettym.to_string ~new_line (Dmarc.Encoder.field ~receiver:domain) value
  in
  output_string oc (Dmarc.field_authentication_results :> string) ;
  output_string oc ": " ;
  output_string oc value ;
  let ic = open_in (Fpath.to_string fpath) in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let rec go buf =
    let len = input ic buf 0 (Bytes.length buf) in
    let str = Bytes.sub_string buf 0 len in
    output_string oc str ;
    if len > 0 then go buf in
  go (Bytes.create 0x7ff)

let verify _quiet newline domain dns fpath output =
  let ic = open_in (Fpath.to_string fpath) in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let buf = Bytes.create 0x7ff in
  let rec go decoder =
    match Dmarc.Verify.decode decoder with
    | #Dmarc.Verify.error as err -> error_msgf "%a" Dmarc.Verify.pp_error err
    | `Info value -> Ok value
    | `Query (decoder, dn, (Dns.Rr_map.K r as k)) ->
        Logs.debug (fun m -> m "ask %a:%a" Dns.Rr_map.ppk k Domain_name.pp dn) ;
        let resp = Dns_static.get_resource_record dns r dn in
        let decoder = Dmarc.Verify.response decoder r resp in
        go decoder
    | `Await decoder -> (
        let len = Stdlib.input ic buf 0 (Bytes.length buf) in
        match len with
        | 0 ->
            let decoder = Dmarc.Verify.src decoder String.empty 0 0 in
            go decoder
        | len when newline = `CRLF ->
            let str = Bytes.sub_string buf 0 len in
            let decoder = Dmarc.Verify.src decoder str 0 len in
            go decoder
        | len ->
            let str = Bytes.sub_string buf 0 len in
            let str = String.split_on_char '\n' str in
            let str = String.concat "\r\n" str in
            let len = String.length str in
            let decoder = Dmarc.Verify.src decoder str 0 len in
            go decoder) in
  let ( let* ) = Result.bind in
  let* info = go (Dmarc.Verify.decoder ()) in
  stamp newline domain fpath info output ;
  Ok ()

let p =
  let open Mrmime in
  let unstructured = Field.(Witness Unstructured) in
  let open Field_name in
  Map.empty
  |> Map.add date unstructured
  |> Map.add from unstructured
  |> Map.add sender unstructured
  |> Map.add reply_to unstructured
  |> Map.add (v "To") unstructured
  |> Map.add cc unstructured
  |> Map.add bcc unstructured
  |> Map.add subject unstructured
  |> Map.add message_id unstructured
  |> Map.add comments unstructured
  |> Map.add content_type unstructured
  |> Map.add content_encoding unstructured

let to_unstrctrd unstructured =
  let fold acc = function #Unstrctrd.elt as elt -> elt :: acc | _ -> acc in
  let unstrctrd = List.fold_left fold [] unstructured in
  Result.get_ok (Unstrctrd.of_list (List.rev unstrctrd))

let pp_version ppf = function
  | Some version -> Fmt.pf ppf "/%d" version
  | None -> ()

let pp_versionned ?longest_uid ppf (str, version) =
  match longest_uid with
  | Some len -> Fmt.pf ppf "%*s%a" len str pp_version version
  | None -> Fmt.pf ppf "%s%a" str pp_version version

let pp_property ppf (t : Dmarc.Authentication_results.property) =
  match t.value with
  | `Value str -> Fmt.pf ppf "%s.%s=%S" t.ty t.property str
  | `Mailbox (None, domain) ->
      Fmt.pf ppf "%s.%s=@%a" t.ty t.property Emile.pp_domain domain
  | `Mailbox (Some local, domain) ->
      let local = String.concat "." local in
      Fmt.pf ppf "%s.%s=%s@%a" t.ty t.property local Emile.pp_domain domain

let pp_properties ppf = function
  | [] -> ()
  | properties ->
      Fmt.pf ppf " @[<hov>(@[%a@])@]"
        Fmt.(list ~sep:(any "@ ") pp_property)
        properties

let show_result ppf (t : Dmarc.Authentication_results.result) =
  let open Dmarc.Authentication_results in
  match (String.lowercase_ascii t.value, Fmt.style_renderer ppf) with
  | "pass", `Ansi_tty ->
      Fmt.pf ppf "%a%a"
        Fmt.(styled `Green pp_versionned)
        (t.meth, t.version) pp_properties t.properties
  | "pass", `None ->
      Fmt.pf ppf "✓ %a%a"
        (pp_versionned ?longest_uid:None)
        (t.meth, t.version) pp_properties t.properties
  | "fail", `Ansi_tty ->
      Fmt.pf ppf "%a%a"
        Fmt.(styled `Red pp_versionned)
        (t.meth, t.version) pp_properties t.properties
  | "fail", `None ->
      Fmt.pf ppf "🞩 %a%a"
        (pp_versionned ?longest_uid:None)
        (t.meth, t.version) pp_properties t.properties
  | str, _ ->
      Fmt.pf ppf "%a=%s%a"
        Fmt.(styled `Yellow pp_versionned)
        (t.meth, t.version) str pp_properties t.properties

let show_results ~longest_uid ppf (t : Dmarc.Authentication_results.t) =
  let open Dmarc.Authentication_results in
  Fmt.pf ppf "%a: @[<v>%a@]\n%!"
    Fmt.(styled `Bold (pp_versionned ~longest_uid))
    (t.servid, t.version)
    Fmt.(list ~sep:(any "@\n") show_result)
    t.results

let collect _quiet newline input =
  let ic, ic_close =
    match input with
    | None -> (stdin, ignore)
    | Some fpath ->
        let ic = open_in (Fpath.to_string fpath) in
        (ic, close_in) in
  let finally () = ic_close ic in
  Fun.protect ~finally @@ fun () ->
  let open Mrmime in
  let decoder = Hd.decoder p in
  let buf = Bytes.create 0x7ff in
  let rec go results =
    match Hd.decode decoder with
    | `Field field ->
        let (Field.Field (fn, w, v)) = Location.prj field in
        let is_authentication_results =
          Field_name.equal Dmarc.field_authentication_results fn in
        begin
          match (is_authentication_results, w) with
          | true, Field.Unstructured ->
              let v = to_unstrctrd v in
              begin
                match Dmarc.Authentication_results.of_unstrctrd v with
                | Ok t -> go (t :: results)
                | Error _ ->
                    Logs.warn (fun m ->
                        m "Invalid Authentication-Results field, ignore it") ;
                    go results
              end
          | _ -> go results
        end
    | `Malformed _ -> error_msgf "Invalid email"
    | `End _ -> Ok (List.rev results)
    | `Await when newline = `CRLF ->
        let len = Stdlib.input ic buf 0 (Bytes.length buf) in
        let str = Bytes.sub_string buf 0 len in
        Hd.src decoder str 0 len ;
        go results
    | `Await ->
        let len = Stdlib.input ic buf 0 (Bytes.length buf) in
        let str = Bytes.sub_string buf 0 len in
        let str = String.split_on_char '\n' str in
        let str = String.concat "\r\n" str in
        let len = String.length str in
        Hd.src decoder str 0 len ;
        go results in
  match go [] with
  | Ok results ->
      let longest_uid =
        let fn acc t =
          let open Dmarc.Authentication_results in
          let str =
            Fmt.str "%a" (pp_versionned ?longest_uid:None) (t.servid, t.version)
          in
          Int.max acc (String.length str) in
        List.fold_left fn 0 results in
      List.iter (show_results ~longest_uid Fmt.stdout) results ;
      `Ok ()
  | Error (`Msg msg) -> `Error (false, Fmt.str "%s." msg)

let verify quiet newline domain resolver fpath output =
  Miou_unix.run ~domains:0 @@ fun () ->
  let daemon, _he, dns = resolver () in
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () =
    Happy_eyeballs_miou_unix.kill daemon ;
    Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  match verify quiet newline domain dns fpath output with
  | Ok () -> `Ok ()
  | Error (`Msg msg) -> `Error (false, Fmt.str "%s." msg)

open Cmdliner
open Args

let input =
  let doc = "The email to check." in
  Arg.(required & pos 0 (some existing_file) None & info [] ~doc)

let setup_resolver happy_eyeballs_cfg nameservers local () =
  let happy_eyeballs =
    match happy_eyeballs_cfg with
    | None -> None
    | Some
        {
          aaaa_timeout;
          connect_delay;
          connect_timeout;
          resolve_timeout;
          resolve_retries;
        } ->
        Happy_eyeballs.create ?aaaa_timeout ?connect_delay ?connect_timeout
          ?resolve_timeout ?resolve_retries
          (Mtime_clock.elapsed_ns ())
        |> Option.some in
  let ( let* ) = Result.bind in
  let daemon, he = Happy_eyeballs_miou_unix.create ?happy_eyeballs () in
  let dns = Dns_static.create ~nameservers ~local he in
  let getaddrinfo dns record domain_name =
    match record with
    | `A ->
        let* ipaddr = Dns_static.gethostbyname dns domain_name in
        Ok Ipaddr.(Set.singleton (V4 ipaddr))
    | `AAAA ->
        let* ipaddr = Dns_static.gethostbyname6 dns domain_name in
        Ok Ipaddr.(Set.singleton (V6 ipaddr)) in
  Happy_eyeballs_miou_unix.inject he (getaddrinfo dns) ;
  (daemon, he, dns)

let setup_resolver =
  let open Term in
  const setup_resolver
  $ setup_happy_eyeballs
  $ setup_nameservers
  $ setup_dns_static

let output =
  let doc =
    "The path of the produced email with the new Authentication-Results field."
  in
  let new_file = Arg.conv (Fpath.of_string, Fpath.pp) in
  Arg.(value & opt (some new_file) None & info [ "o"; "output" ] ~doc)

let hostname =
  let parser =
    Angstrom.(parse_string ~consume:Consume.All) Emile.Parser.domain in
  let parser str =
    match parser str with
    | Ok _ as value -> value
    | Error _ -> error_msgf "Invalid domain: %S" str in
  let pp = Emile.pp_domain in
  Arg.conv (parser, pp)

let generate ~len =
  let res = Bytes.make len '\000' in
  for i = 0 to len - 1 do
    let chr =
      match Random.int (26 + 26 + 10) with
      | n when n < 26 -> Char.unsafe_chr (65 + n)
      | n when n < 26 + 26 -> Char.unsafe_chr (97 + n - 26)
      | n -> Char.unsafe_chr (48 + n - 26 - 26) in
    Bytes.set res i chr
  done ;
  Bytes.unsafe_to_string res

let default_hostname =
  let str = Unix.gethostname () in
  match (fst hostname) str with
  | `Ok domain -> domain
  | `Error _ ->
      let[@warning "-8"] (`Ok random_hostname : [ `Ok of _ | `Error of _ ]) =
        (fst hostname) (generate ~len:16) in
      Logs.warn (fun m ->
          m "Invalid default hostname: %S, use %a as the default hostname" str
            Emile.pp_domain random_hostname) ;
      random_hostname

let hostname =
  let doc = "Domain name of the machine." in
  let open Arg in
  value & opt hostname default_hostname & info [ "h"; "hostname" ] ~doc

let verify =
  let doc = "Verify DMARC informations & stamp an email with results." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) verifies DMARC informations & stamps the given email with \
         results.";
    ] in
  let open Term in
  let info = Cmd.info "verify" ~doc ~man in
  let term =
    const verify
    $ setup_logs
    $ newline
    $ hostname
    $ setup_resolver
    $ input
    $ output in
  Cmd.v info (ret term)

let existing_file =
  let parser = function
    | "-" -> Ok None
    | str ->
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok (Some v)
    | Ok v -> error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.option ~none:(Fmt.any "-") Fpath.pp)

let input =
  let doc = "The email to analyze." in
  Arg.(value & pos 0 existing_file None & info [] ~doc)

let collect =
  let doc = "Collect Authentication-Results fields and show them." in
  let man = [ `S Manpage.s_description ] in
  let open Term in
  let info = Cmd.info "collect" ~doc ~man in
  let term = const collect $ setup_logs $ newline $ input in
  Cmd.v info (ret term)

let default = Term.(ret (const (`Help (`Pager, None))))

let cmd =
  let doc = "A DMARC tool." in
  let man = [ `S Manpage.s_description ] in
  Cmd.group ~default (Cmd.info "dmarc" ~doc ~man) [ verify; collect ]
