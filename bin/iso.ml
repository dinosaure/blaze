let to_output_channel_from_filename filename t oc =
  let fd = Unix.openfile (Fpath.to_string filename) Unix.[ O_RDONLY ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let map ~off ~len =
    Logs.debug (fun m -> m "map off:%08x len:%d" off len) ;
    let barr =
      Unix.map_file fd ~pos:(Int64.of_int off) Bigarray.char Bigarray.c_layout
        false [| len |] in
    Bigarray.array1_of_genarray barr in
  let load (pos, pos_end) = map ~off:pos ~len:(pos_end - pos) in
  let seq = Email.to_seq ~load t in
  let fn = function
    | `String str -> output_string oc str
    | `Value bstr -> Email.output_bigstring oc bstr in
  Seq.iter fn seq

let run _quiet filename output =
  let filename = Fpath.v filename in
  match Email.of_filename filename with
  | Ok (t, _) ->
      let oc, finally =
        match output with
        | Some filename ->
            let oc = open_out (Fpath.to_string filename) in
            let finally () = close_out oc in
            (oc, finally)
        | None -> (stdout, ignore) in
      Fun.protect ~finally @@ fun () ->
      to_output_channel_from_filename filename t oc ;
      `Ok ()
  | Error `Invalid -> `Error (false, "Invalid email")
  | Error `No_symmetry ->
      `Error (false, "No symmetry between Mr.MIME and our skeleton")
  | Error `Not_enough -> `Error (false, "Not enough input for an email")

open Cmdliner
open Blaze_cli

let input =
  let doc = "The incoming email." in
  let open Arg in
  required & pos 0 (some file) None & info [] ~doc ~docv:"FILE"

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
