open Rresult

let stream_of_in_channel ic () =
  match input_line ic with
  | line -> Some (line ^ "\r\n", 0, String.length line + 2)
  | exception End_of_file -> None

let pp_protocol : Received.protocol Fmt.t =
 fun ppf v ->
  match (v :> [ `Atom of string | `ESMTP | `SMTP ]) with
  | `Atom v -> Fmt.string ppf v
  | `ESMTP -> Fmt.string ppf "ESMTP"
  | `SMTP -> Fmt.string ppf "SMTP"

let pp_link : Received.link Fmt.t =
 fun ppf v ->
  match (v :> [ `Atom of string | `TCP ]) with
  | `Atom v -> Fmt.string ppf v
  | `TCP -> Fmt.string ppf "tcp"

let show_one recv =
  let _by = Received.received_by recv in
  let _from = Received.received_from recv in
  let _for = Received.received_for recv in
  let () =
    match (_by, _from, _for) with
    | ( Some (Received.Only _by | Received.With (_by, _)),
        Some (Received.Only _from | Received.With (_from, _)),
        Some _for ) ->
        Fmt.pr "from:%a -> by:%a -> for:%a\n%!" Colombe.Domain.pp _from
          Colombe.Domain.pp _by Colombe.Path.pp _for
    | Some (Received.Only _by | Received.With (_by, _)), None, Some _for ->
        Fmt.pr "by:%a -> for:%a\n%!" Colombe.Domain.pp _by Colombe.Path.pp _for
    | Some (Received.Only _by | Received.With (_by, _)), None, None ->
        Fmt.pr "by:%a\n%!" Colombe.Domain.pp _by
    | _ -> () in
  let _with = Received.received_with recv in
  let via = Received.received_via recv in
  let () =
    match (_with, via) with
    | Some protocol, Some link ->
        Fmt.pr "\twith %a\n%!" pp_protocol protocol ;
        Fmt.pr "\tvia %a\n%!" pp_link link
    | Some protocol, None -> Fmt.pr "\twith %a\n%!" pp_protocol protocol
    | None, Some link -> Fmt.pr "  vi %a\n%!" pp_link link
    | _ -> () in
  ()

let show recvs = List.iter show_one recvs

module Gr = Graph.Imperative.Digraph.Concrete (struct
  include Colombe.Domain

  let hash = Hashtbl.hash
end)

module Dot = Graph.Graphviz.Dot (struct
  include Gr

  let vertex_name v = Fmt.str "%a" Fmt.(quote Colombe.Domain.pp) v
  let edge_attributes _ = [ `Color 0xffffff ]
  let default_edge_attributes _ = []

  let vertex_attributes _ =
    [ `Color 0xffffff; `Fontcolor 0xffffff; `Shape `Box ]

  let default_vertex_attributes _ = []
  let graph_attributes _ = [ `BgcolorWithTransparency 0x0l; `Ratio `Compress ]
  let get_subgraph _ = None
end)

let make_graph recvs =
  let g = Gr.create ~size:(List.length recvs) () in
  let add recv =
    match (Received.received_from recv, Received.received_by recv) with
    | ( Some (Received.Only from | Received.With (from, _)),
        Some (Received.Only _by | Received.With (_by, _)) ) ->
        Gr.add_edge g from _by
    | Some (Received.Only from | Received.With (from, _)), None ->
        Gr.add_vertex g from
    | None, Some (Received.Only _by | Received.With (_by, _)) ->
        Gr.add_vertex g _by
    | _ -> () in
  List.iter add recvs ;
  g

let show_graph g =
  Dot.output_graph stdout g ;
  flush stdout

let extract dot input =
  let ic, close =
    match input with
    | "-" -> (stdin, ignore)
    | filename -> (open_in filename, close_in) in
  let stream = stream_of_in_channel ic in
  match
    Received.of_stream stream >>| fun res ->
    close ic ;
    res
  with
  | Ok (_prelude, recvs) when dot ->
      let g = make_graph recvs in
      show_graph g ;
      `Ok ()
  | Ok (_prelude, recvs) ->
      let recvs = List.sort Received.compare recvs in
      show recvs ;
      `Ok ()
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

let tmp = Bytes.create 65536

let rec pipe ic oc =
  match input ic tmp 0 (Bytes.length tmp) with
  | 0 -> ()
  | len ->
      output_substring oc (Bytes.unsafe_to_string tmp) 0 len ;
      pipe ic oc
  | exception End_of_file -> ()

let by hostname =
  match Colombe.Domain.of_string hostname with
  | Ok v -> Received.Only v
  | Error (`Msg _) -> Fmt.failwith "Invalid hostname: %S" hostname

let stamp hostname zone from _for input =
  let link = Received.link "UUCP" in
  let by = by hostname in
  let protocol =
    match Fmt.utf_8 Fmt.stdout with
    | false -> Received.protocol "LMTP"
    | true -> Received.protocol "UTF8LMTP" in
  let id =
    None
    (* TODO(dinosaure): see [maildir]. *) in
  let stamp =
    Received.make ~from:(Received.Only from) ~by ~via:link ~protocol ?id
      (Some _for) ~zone (Ptime_clock.now ()) in
  Fmt.pr "%s%!"
    (Prettym.to_string ~new_line:"\n" Received.Encoder.as_field stamp) ;
  match input with
  | "-" ->
      pipe stdin stdout ;
      `Ok ()
  | filename ->
      let ic = open_in filename in
      pipe ic stdout ;
      close_in ic ;
      `Ok ()

open Cmdliner

let hostname =
  let parser str =
    match Colombe.Domain.of_string str with
    | Ok _ -> Ok str
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.string)

let zone = Arg.conv (Mrmime.Date.Zone.of_string, Mrmime.Date.Zone.pp)
let domain = Arg.conv (Colombe.Domain.of_string, Colombe.Domain.pp)

let path =
  let parser str =
    Emile.of_string str
    |> R.reword_error (fun _ -> R.msg "Invalid path")
    >>= Colombe_emile.to_path in
  Arg.conv (parser, Colombe.Path.pp)

let input =
  let doc = "The email to analyze. Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos 1 Args.file "-" & info [] ~doc)

let dot =
  let doc = "Print a $(i,dot) graph." in
  Arg.(value & flag & info [ "d"; "dot" ] ~doc)

let hostname =
  let doc = "Specify the hostname used to stamp the given email." in
  Arg.(
    value & opt hostname (Unix.gethostname ()) & info [ "h"; "hostname" ] ~doc)

let zone =
  let doc = "Specify the time zone used to stamp the given email." in
  Arg.(value & opt zone Mrmime.Date.Zone.UT & info [ "z"; "zone" ] ~doc)

let _for =
  let doc = "The recipient of the given email." in
  Arg.(required & pos 0 (some path) None & info [] ~doc)

let from =
  let doc = "The domain which identifies the source of the given email." in
  Arg.(required & opt (some domain) None & info [ "f"; "from" ] ~doc)

let stamp =
  let doc = "Stamp the given email with a new Received field." in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) stamps the given $(i,msgs) with a new $(i,Received) field.";
    ] in
  Cmd.v
    (Cmd.info "stamp" ~doc ~man)
    Term.(ret (const stamp $ hostname $ zone $ from $ _for $ input))

let input =
  let doc = "The email to analyze. Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos 0 Args.file "-" & info [] ~doc)

let extract =
  let doc = "Extract Received fields" in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) prints $(i,Received) fields from the specified $(i,msgs).";
    ] in
  Cmd.v (Cmd.info "extract" ~doc ~man) Term.(ret (const extract $ dot $ input))

let default = Term.(ret (const (`Help (`Pager, None))))

let cmd =
  let doc = "A tool to manipulate Received fields." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Use $(tname) $(i,extract) to extract $(i,Received) informations from \
         the given $(i,msgs).";
      `P
        "Use $(tname) $(i,stamp) to stamp the given $(i,msgs) with a new \
         $(i,Received) field.";
    ] in
  Cmd.group ~default (Cmd.info "recv" ~doc ~man) [ extract; stamp ]
