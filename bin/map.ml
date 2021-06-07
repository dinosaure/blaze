open Rresult

let const x _ = x

let ( <.> ) f g x = f (g x)

let emitter_of_queue q = function Some str -> Queue.push str q | None -> ()

let stream_of_queue q () =
  match Queue.pop q with
  | v -> Some (v, 0, String.length v)
  | exception Queue.Empty -> None

let blit src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let empty_part = Mrmime.Mt.part (const None)

let parser ic =
  let uid = ref (-1) in
  let tbl = Hashtbl.create 0x10 in
  let emitters _header =
    incr uid ;
    let v = !uid in
    let contents = Queue.create () in
    Hashtbl.add tbl v contents ;
    (emitter_of_queue contents, v) in
  let parser = Mrmime.Mail.stream ~emitters in
  let rec loop ic ke = function
    | Angstrom.Unbuffered.Done (_, (header, mail)) -> R.ok (header, mail, tbl)
    | Fail _ -> R.error_msgf "Invalid incoming email"
    | Partial { committed; continue } -> (
        Ke.Rke.N.shift_exn ke committed ;
        if committed = 0 then Ke.Rke.compress ke ;
        match input_line ic with
        | line ->
            Ke.Rke.N.push ke ~blit ~length:String.length ~off:0
              ~len:(String.length line) line ;
            Ke.Rke.push ke '\r' ;
            Ke.Rke.push ke '\n' ;
            let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
            loop ic ke
              (continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete)
        | exception End_of_file ->
            let buf =
              match Ke.Rke.length ke with
              | 0 -> Bigstringaf.empty
              | _ ->
                  Ke.Rke.compress ke ;
                  List.hd (Ke.Rke.N.peek ke) in
            loop ic ke
              (continue buf ~off:0 ~len:(Bigstringaf.length buf) Complete))
  in
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  loop ic ke (Angstrom.Unbuffered.parse parser)

let encoder _header mail tbl =
  let rec go = function
    | Mrmime.Mail.Leaf { header; body } ->
        let queue = Hashtbl.find tbl body in
        let stream = stream_of_queue queue in
        Mrmime.Mt.part ~header stream
    | Mrmime.Mail.Multipart { header; body } ->
        let f = function Some v -> go v | None -> empty_part in
        let parts = List.map f body in
        Mrmime.Mt.multipart ~rng:Mrmime.Mt.rng ~header parts
        |> Mrmime.Mt.multipart_as_part
    | _ -> failwith "Not implemented yet!"
    (* TODO *) in
  Mrmime.Mt.make Mrmime.Header.empty Mrmime.Mt.simple (go mail)

let crlf = Astring.String.Sub.v "\r\n"

let rec transmit state oc stream =
  match stream () with
  | Some (_, _, 0) -> transmit state oc stream
  | Some (str, off, len) when state = `CR && str.[0] = '\n' -> (
      output_char oc '\n' ;
      let lines =
        List.map Astring.String.Sub.to_string
          Astring.String.(
            Sub.cuts ~sep:crlf (sub_with_range ~first:(off + 1) ~len str)) in
      let lines = String.concat "\n" lines in
      match str.[off + len - 1] with
      | '\r' ->
          output_substring oc lines 0 (String.length lines - 1) ;
          transmit `CR oc stream
      | _ ->
          output_string oc lines ;
          transmit `None oc stream)
  | Some (str, off, len) -> (
      let lines =
        List.map Astring.String.Sub.to_string
          Astring.String.(
            Sub.cuts ~sep:crlf (sub_with_range ~first:off ~len str)) in
      let lines = String.concat "\n" lines in
      match str.[off + len - 1] with
      | '\r' ->
          output_substring oc lines 0 (String.length lines - 1) ;
          transmit `CR oc stream
      | _ ->
          output_string oc lines ;
          transmit `None oc stream)
  | None -> ()

let map _ diff input output =
  let ic, close_ic =
    match input with
    | Some fpath -> (open_in (Fpath.to_string fpath), close_in)
    | None -> (stdin, ignore) in
  let v = parser ic in
  close_ic ic ;
  match (v, diff) with
  | Ok (header, mail, tbl), false ->
      let mail' = encoder header mail tbl in
      let oc, close_oc =
        match output with
        | Some fpath -> (open_out (Fpath.to_string fpath), close_out)
        | None -> (stdout, ignore) in
      transmit `None oc (Mrmime.Mt.to_stream mail') ;
      close_oc oc ;
      `Ok 0
  | Ok _, _ -> assert false
  | Error (`Msg err), _ -> `Error (false, Fmt.str "%s." err)

open Cmdliner

let common_options = "COMMON OPTIONS"

let verbosity =
  let env = Arg.env_var "BLAZE_LOGS" in
  Logs_cli.level ~docs:common_options ~env ()

let renderer =
  let env = Arg.env_var "BLAZE_FMT" in
  Fmt_cli.style_renderer ~docs:common_options ~env ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr) ;
  Option.is_none level

let setup_logs = Term.(const setup_logs $ renderer $ verbosity)

let existing_file =
  let parser = function
    | "-" -> Ok None
    | str ->
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok (Some v)
    | Ok v -> Rresult.R.error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.option ~none:(Fmt.any "-") Fpath.pp)

let input =
  let doc = "The email to decode & encode." in
  Arg.(value & pos 0 existing_file None & info [] ~doc)

let new_file = Arg.conv (Fpath.of_string, Fpath.pp)

let output =
  let doc = "The path of the encoded email." in
  Arg.(value & opt (some new_file) None & info [ "o"; "output" ] ~doc)

let diff =
  let doc =
    "Instead to show the encoded email, we show the diff from the source." in
  Arg.(value & flag & info [ "diff" ] ~doc)

let map =
  let doc = "Try to decode and encode the given message." in
  let man =
    [
      `S Manpage.s_description;
      `P "From the given email, we try to decode and encode it.";
    ] in
  ( Term.(ret (const map $ setup_logs $ diff $ input $ output)),
    Term.info "map" ~doc ~man )

let () = Term.(exit_status @@ eval map)
