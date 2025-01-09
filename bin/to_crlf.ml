let run _quiet filename output =
  let oc, finally =
    match output with
    | Some filename ->
        let oc = open_out (Fpath.to_string filename) in
        let finally () = close_out oc in
        (oc, finally)
    | None -> (stdout, ignore) in
  Fun.protect ~finally @@ fun () ->
  let ic = open_in (Fpath.to_string filename) in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let rec go () =
    match input_line ic with
    | line ->
        output_string oc line ;
        output_char oc '\r' ;
        output_char oc '\n' ;
        go ()
    | exception End_of_file -> () in
  go ()

open Cmdliner
open Args

let input =
  let doc = "The email uses the system line feed." in
  let open Arg in
  required & pos 0 (some existing_file) None & info [] ~doc ~docv:"FILE"

let output =
  let doc = "The output of the $(tname) program." in
  let parser str =
    match Fpath.of_string str with
    | Ok value when Sys.file_exists str ->
        error_msgf "%a already exists" Fpath.pp value
    | Ok _ as value -> value
    | Error _ as err -> err in
  let non_existing_filename = Arg.conv (parser, Fpath.pp) in
  let open Arg in
  value
  & opt (some non_existing_filename) None
  & info [ "o"; "output" ] ~doc ~docv:"FILE"

let term =
  let open Term in
  const run $ setup_logs $ input $ output

let cmd =
  let doc =
    "Transforms an email using the system's line feed into an email compatible \
     with the SMTP protocol." in
  let man = [] in
  let info = Cmd.info "to_crlf" ~doc ~man in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
