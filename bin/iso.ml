let run _quiet filename output =
  match Trim.of_filename filename with
  | Ok t ->
      let oc, finally =
        match output with
        | Some filename ->
            let oc = open_out (Fpath.to_string filename) in
            let finally () = close_out oc in
            (oc, finally)
        | None -> (stdout, ignore) in
      Fun.protect ~finally @@ fun () ->
      Trim.to_output_channel_from_filename filename t oc ;
      `Ok ()
  | Error (`Msg msg) -> `Error (false, Fmt.str "%s." msg)

open Cmdliner
open Args

let input =
  let doc = "The incoming email." in
  let parser str =
    match Fpath.of_string str with
    | Ok value when Sys.file_exists str && Sys.is_directory str = false ->
        Ok value
    | Ok value -> error_msgf "%a does not exist" Fpath.pp value
    | Error _ as err -> err in
  let existing_filename = Arg.conv (parser, Fpath.pp) in
  let open Arg in
  required & pos 0 (some existing_filename) None & info [] ~doc ~docv:"FILE"

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
  ret (const run $ setup_logs $ input $ output)

let cmd =
  let doc = "Explode and reconstruct the given email." in
  let man = [] in
  let info = Cmd.info "iso" ~doc ~man in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
