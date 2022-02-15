open Rresult
module Lwt_scheduler = Dmarc.Sigs.Make (Lwt)

module Flow = struct
  type flow = Lwt_unix.file_descr
  type +'a io = 'a Lwt.t

  let input = Lwt_unix.read
end

module DNS = struct
  type t = Ldns.t
  and +'a io = 'a Lwt.t

  and error =
    [ `Msg of string
    | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
    | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

  let getrrecord dns response domain_name =
    let res = Ldns.get_resource_record dns response domain_name in
    Lwt.return res
end

include
  Dmarc.Make
    (Lwt_scheduler)
    (struct
      include Lwt

      let iter_p = Lwt_list.iter_p
      let map_p = Lwt_list.map_p
    end)
    (Flow)
    (DNS)

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

let always x _ = x

let verify local nameservers timeout sender helo ip input =
  let open Lwt.Infix in
  (match input with
  | Some fpath ->
      Lwt_unix.openfile (Fpath.to_string fpath) Unix.[ O_RDONLY ] 0o600
      >>= fun fd -> Lwt.return (fd, Lwt_unix.close)
  | None -> Lwt.return (Lwt_unix.stdin, always Lwt.return_unit))
  >>= fun (fd, close) ->
  let epoch () = Int64.of_float (Unix.gettimeofday ()) in
  let ctx = ctx sender helo ip in
  let dns = Ldns.create ?nameservers ~timeout ~local () in
  verify ~newline:LF ~ctx ~epoch dns fd >>= fun res ->
  close fd >>= fun () -> Lwt.return res

let pp_spf_aligned ppf = function
  | true -> Fmt.pf ppf "%a" Fmt.(styled `Green string) "aligned"
  | false -> Fmt.pf ppf "%a" Fmt.(styled `Yellow string) "unaligned"

let pp_spf_result ppf = function
  | Ok (_ctx, `Pass _) -> Fmt.(styled `Green string) ppf "pass"
  | Ok (_ctx, v) -> Fmt.(styled `Red Uspf.pp_res) ppf v
  | Error (_, _msg) -> Fmt.(styled `Red string) ppf "internal error"

let pp_dkim_result ppf = function
  | Ok (`Valid dkim) ->
      Fmt.(styled `Green Domain_name.pp) ppf (Dkim.domain dkim)
  | Ok (`Invalid dkim) ->
      Fmt.(styled `Red Domain_name.pp) ppf (Dkim.domain dkim)
  | Error (`DKIM_record_unreachable dkim) ->
      Fmt.pf ppf "%a:unreachable"
        Fmt.(styled `Red Domain_name.pp)
        (Dkim.domain dkim)
  | Error (`Invalid_DKIM_record (dkim, _)) ->
      Fmt.pf ppf "%a:invalid-record"
        Fmt.(styled `Red Domain_name.pp)
        (Dkim.domain dkim)

let show_result = function
  | Ok (`Pass (_spf_aligned, _spf, domain_name)) ->
      Fmt.pr "%a: %a\n%!" Domain_name.pp domain_name
        Fmt.(styled `Green string)
        "pass"
  | Ok (`Fail (spf_aligned, spf, dkims)) ->
      Fmt.pr "%a: %a, SPF:%a, DKIM:%a\n%!"
        Fmt.(styled `Red string)
        "error" pp_spf_aligned spf_aligned pp_spf_result spf
        Fmt.(Dump.list pp_dkim_result)
        dkims
  | Error err ->
      Fmt.pr "%a: %a\n%!" Fmt.(styled `Red string) "error" Dmarc.pp_error err

let to_exit_code = function
  | Ok (`Pass _) -> `Ok 0
  | Ok (`Fail _) -> `Ok 1
  | Error err -> `Error (false, Fmt.str "%a." Dmarc.pp_error err)

let verify quiet local nameservers timeout sender helo ip input =
  let res =
    Lwt_main.run (verify local nameservers timeout sender helo ip input) in
  match quiet with
  | false ->
      show_result res ;
      `Ok 0
  | true -> to_exit_code res

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

let cmd =
  let doc = "Verify SPF & DKIM and report DMARC result." in
  let man =
    [
      `S Manpage.s_description; `P "Verify SPF & DKIM and report DMARC result.";
    ] in
  ( Term.(
      ret
        (const verify
        $ setup_logs
        $ setup_local_dns
        $ nameserver
        $ timeout
        $ sender
        $ helo
        $ ip
        $ input)),
    Term.info "dmarc" ~doc ~man )

let () = Term.(exit_status @@ eval cmd)
