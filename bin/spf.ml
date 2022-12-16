open Rresult

module Caml_scheduler = Uspf.Sigs.Make (struct
  type 'a t = 'a
end)

let state =
  let open Uspf.Sigs in
  let open Caml_scheduler in
  { return = (fun x -> inj x); bind = (fun x f -> f (prj x)) }

module DNS = struct
  type t = Ldns.t
  and backend = Caml_scheduler.t

  and error =
    [ `Msg of string
    | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
    | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

  let getrrecord dns response domain_name =
    Caml_scheduler.inj @@ Ldns.get_resource_record dns response domain_name
end

module Flow = struct
  type flow = in_channel
  and backend = Caml_scheduler.t

  let input ic tmp off len = Caml_scheduler.inj @@ input ic tmp off len
end

let ctx sender helo ip =
  Uspf.empty |> fun ctx ->
  Option.fold ~none:ctx
    ~some:(fun helo -> Uspf.with_sender (`HELO helo) ctx)
    helo
  |> fun ctx ->
  Option.fold ~none:ctx
    ~some:(fun sender -> Uspf.with_sender (`MAILFROM sender) ctx)
    sender
  |> fun ctx -> Option.fold ~none:ctx ~some:(fun ip -> Uspf.with_ip ip ctx) ip

let rec transmit ic oc =
  let tmp = Bytes.create 0x1000 in
  go tmp ic oc

and go tmp ic oc =
  let len = input ic tmp 0 (Bytes.length tmp) in
  if len > 0
  then (
    output oc tmp 0 len ;
    go tmp ic oc)

let unstrctrd_to_utf_8_string_with_lf l =
  let buf = Buffer.create 0x100 in
  let f = function
    | `CR -> Buffer.add_char buf '\r'
    | `FWS (wsp : Unstrctrd.wsp) ->
        Buffer.add_char buf '\n' ;
        Buffer.add_string buf (wsp :> string)
    | `LF | `Invalid_char _ -> ()
    | `OBS_NO_WS_CTL (chr : Unstrctrd.obs) -> Buffer.add_char buf (chr :> char)
    | `Uchar uchar -> Uutf.Buffer.add_utf_8 buf uchar
    | `WSP (wsp : Unstrctrd.wsp) -> Buffer.add_string buf (wsp :> string)
    | `d0 -> Buffer.add_char buf '\000' in
  Unstrctrd.iter ~f l ;
  Buffer.contents buf

let check local ?nameservers ~timeout ctx =
  let dns = Ldns.create ?nameservers ~timeout ~local () in
  Uspf.get ~ctx state dns (module DNS) |> Caml_scheduler.prj >>| fun record ->
  Uspf.check ~ctx state dns (module DNS) record |> Caml_scheduler.prj

let extract_received_spf ?newline ic =
  Uspf.extract_received_spf ?newline ic state (module Flow)
  |> Caml_scheduler.prj

let stamp quiet local nameservers timeout hostname sender helo ip input output =
  let ic, close_ic =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let oc, close_oc =
    match output with
    | Some fpath -> (open_out (Fpath.to_string fpath), close_out)
    | None -> (stdout, ignore) in
  let ctx = ctx sender helo ip in
  match check ?nameservers ~timeout local ctx with
  | Ok res when quiet -> (
      match res with
      | `Pass _ | `None | `Neutral -> `Ok 0
      | `Fail | `Softfail | `Permerror | `Temperror -> `Ok 1)
  | Ok res ->
      let field_name, unstrctrd = Uspf.to_field ~ctx ~receiver:hostname res in
      Fmt.pr "%a: %s\n%!" Mrmime.Field_name.pp field_name
        (unstrctrd_to_utf_8_string_with_lf unstrctrd) ;
      transmit ic oc ;
      close_ic ic ;
      close_oc oc ;
      let res =
        match res with
        | `Pass _ | `None | `Neutral -> 0
        | _ (* Fail | Softfail | Permerror | Temperror *) -> 1 in
      `Ok res
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

let to_exit_code results =
  let res = ref true in
  let f (_, _, a, b) =
    match (a, b) with
    | `Pass, `Pass _
    | `Fail, `Fail
    | `None, `None
    | `Permerror, `Permerror
    | `Temperror, `Temperror
    | `Neutral, `Neutral
    | `Softfail, `Softfail ->
        ()
    | _ -> res := false in
  List.iter f results ;
  if !res then `Ok 0 else `Ok 1

let pp_expected ppf = function
  | `Pass -> Fmt.(styled `Green string) ppf "pass"
  | `Fail -> Fmt.(styled `Red string) ppf "fail"
  | `None -> Fmt.(styled `Faint string) ppf "none"
  | `Neutral -> Fmt.(styled `Blue string) ppf "neutral"
  | `Softfail -> Fmt.(styled `Yellow string) ppf "softfail"
  | `Permerror -> Fmt.(styled `Red string) ppf "permerror"
  | `Temperror -> Fmt.(styled `Red string) ppf "temperror"

let pp_result ppf = function
  | `Pass _ -> Fmt.(styled `Green string) ppf "pass"
  | `Fail -> Fmt.(styled `Red string) ppf "fail"
  | `None -> Fmt.(styled `Faint string) ppf "none"
  | `Neutral -> Fmt.(styled `Blue string) ppf "neutral"
  | `Softfail -> Fmt.(styled `Yellow string) ppf "softfail"
  | `Permerror -> Fmt.(styled `Red string) ppf "permerror"
  | `Temperror -> Fmt.(styled `Red string) ppf "temperror"

let show_results results =
  let f = function
    | Some sender, Some ip, expected, result ->
        Fmt.pr "%a from %a: %a (expected: %a)\n%!" Emile.pp_mailbox sender
          Ipaddr.pp ip pp_result result pp_expected expected
    | Some sender, None, expected, result ->
        Fmt.pr "%a: %a (expected: %a)\n%!" Emile.pp_mailbox sender pp_result
          result pp_expected expected
    | None, Some ip, expected, result ->
        Fmt.pr "%a: %a (expected: %a)\n%!" Ipaddr.pp ip pp_result result
          pp_expected expected
    | None, None, expected, result ->
        Fmt.pr "unidentified sender: %a (expected %a)\n%!" pp_result result
          pp_expected expected in
  List.iter f results ;
  `Ok 0
(* XXX(dinosaure): [to_exit_codes results]? *)

let analyze quiet local nameservers timeout input =
  let ic, close_ic =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let res = extract_received_spf ~newline:Uspf.LF ic in
  close_ic ic ;
  match res with
  | Ok extracted ->
      let results =
        List.fold_left
          (fun acc { Uspf.result; ctx; sender; ip; _ } ->
            match check ?nameservers ~timeout local ctx with
            | Ok v -> (sender, ip, result, v) :: acc
            | _ -> acc)
          [] extracted in
      if quiet then to_exit_code results else show_results results
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

open Cmdliner
open Args

let existing_file =
  let parser = function
    | "-" -> Ok None
    | str ->
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok (Some v)
    | Ok v -> R.error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.option ~none:(Fmt.any "-") Fpath.pp)

let input =
  let doc = "The email to check." in
  Arg.(value & pos 0 existing_file None & info [] ~doc)

let new_file = Arg.conv (Fpath.of_string, Fpath.pp)

let output =
  let doc = "The path of the produced email with the new Received-SPF field." in
  Arg.(value & opt (some new_file) None & info [ "o"; "output" ] ~doc)

let domain =
  let parser str =
    match Angstrom.parse_string ~consume:All Emile.Parser.domain str with
    | Ok v -> Ok v
    | Error _ -> R.error_msgf "Invalid domain: %S" str in
  let pp = Emile.pp_domain in
  Arg.conv (parser, pp)

let domain =
  let doc = "The hostname of the computer." in
  Arg.(
    value
    & opt domain (R.get_ok (Arg.conv_parser domain (Unix.gethostname ())))
    & info [ "h"; "hostname" ] ~doc)

let sender =
  let parser str =
    match R.(Emile.of_string str >>= Colombe_emile.to_path) with
    | Ok v -> Ok v
    | Error _ -> R.error_msgf "Invalid sender: %S" str in
  let pp = Colombe.Path.pp in
  Arg.conv (parser, pp)

let sender =
  let doc = "The sender of the given email." in
  Arg.(value & opt (some sender) None & info [ "s"; "sender" ] ~doc)

let domain_name = Arg.conv (Domain_name.of_string, Domain_name.pp)

let helo =
  let doc = "HELO/EHLO name used by the SMTP client." in
  Arg.(value & opt (some domain_name) None & info [ "helo" ] ~doc)

let ip =
  let doc = "The IP address of the client." in
  let ipaddr = Arg.conv (Ipaddr.of_string, Ipaddr.pp) in
  Arg.(value & opt (some ipaddr) None & info [ "ip" ] ~doc)

let stamp =
  let doc =
    "Stamps the given message with the Received-SPF field and its result." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Stamps the given email with a new Received-SPF field and its result \
         from given arguments (such as the $(i,ip) address and the \
         $(i,sender)).";
    ] in
  Cmd.v
    (Cmd.info "stamp" ~doc ~man)
    Term.(
      ret
        (const stamp
        $ setup_logs
        $ setup_local_dns
        $ nameserver
        $ timeout
        $ domain
        $ sender
        $ helo
        $ ip
        $ input
        $ output))

let analyze =
  let doc =
    "Analyzes Received-SPF fields from the given message and shows their \
     results and the reproductibility of these results." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Analyzes the given email and extract Received-SPF fields to reproduce \
         expected results.";
    ] in
  Cmd.v
    (Cmd.info "analyze" ~doc ~man)
    Term.(
      ret
        (const analyze
        $ setup_logs
        $ setup_local_dns
        $ nameserver
        $ timeout
        $ input))

let default = Term.(ret (const (`Help (`Pager, None))))

let () =
  let doc = "A tool to manipulate Received-SPF fields." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Use $(tname) $(i,stamp) to stamp the incoming email with an \
         Received-SPF result.";
      `P
        "Use $(tname) $(i,analyze) to check Received-SPF fields from the \
         incoming email.";
    ] in
  let cmd = Cmd.group ~default (Cmd.info "spf" ~doc ~man) [ stamp; analyze ] in
  Cmd.(exit @@ eval' cmd)
