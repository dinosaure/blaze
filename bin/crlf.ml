let run _quiet filename output =
  let oc, finally =
    match output with
    | Some filename ->
        let oc = open_out (Fpath.to_string filename) in
        let finally () = close_out oc in
        (oc, finally)
    | None -> (stdout, ignore) in
  Fun.protect ~finally @@ fun () ->
  let ic = open_in filename in
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
open Blaze_cli

let input =
  let doc = "The email which uses the $(i,lf) line feed." in
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
  const run $ setup_logs $ input $ output

let cmd =
  let doc =
    "Transforms an email using the $(i,lf) line feed into an email compatible \
     with the SMTP protocol (the $(i,crlf) line feed)." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) reads a file line by line (where the line feed is $(i,lf)) \
         and re-emits the lines but with a new line feed: $(i,crlf) (the one \
         used in particular by the SMTP protocol).";
      `P
        "It is preferable to handle emails using the $(i,crlf) line feed, as \
         this is also the line feed described in $(i,RFC822) (which describes \
         the format of emails).";
      `P
        "For example, the $(b,blaze) archiving system uses the $(i,crlf) line \
         feed rather than $(i,lf). Verification tools such as DKIM also rely \
         on the $(i,crlf) line feed.";
      `P
        "There are cases where the $(b,LF) character can have real \
         significance for the user but has no significance in terms of \
         $(i,RFC822). A parallel can be drawn with the fact that $(b,LF) has \
         no particular significance in an HTML document (except in a few \
         exceptional cases) but $(b,<br />) has significance in the display of \
         the document. For emails, it's the same: $(i,lf) can have meaning for \
         the user, whereas $(i,crlf) is completely transparent to the user. We \
         can therefore find emails with $(b,<CRLF><LF><CRLF>) where our second \
         $(b,<LF>) is part of the email content.";
    ] in
  let info = Cmd.info "crlf" ~doc ~man in
  Cmd.v info term
