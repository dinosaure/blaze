open Cmdliner

let default =
  let open Term in
  ret (const (`Help (`Pager, None)))

let () =
  let doc = "A swiss-army knife for emails." in
  let man = [] in
  let info = Cmd.info "blaze" ~doc ~man in
  let cmd =
    Cmd.group ~default info
      [
        Addr.cmd;
        Blaze_dkim.cmd;
        Srv.cmd;
        Descr.cmd;
        Send.cmd;
        Fetch.cmd;
        Submit.cmd;
        Make.cmd;
        Rand.cmd;
        Crlf.cmd;
        Iso.cmd;
        Blaze_mbox.cmd;
        Blaze_pack.cmd;
        Hdr.cmd;
        Mdir.cmd;
        Map.cmd;
        Spf.cmd;
        Recv.cmd;
        Blaze_dmarc.cmd;
      ] in
  Cmd.(exit (eval cmd))
