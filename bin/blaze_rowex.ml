let value_of_uid_and_offset uid offset =
  if uid > 0x7fff then invalid_arg "Too big pack identifier" ;
  if offset > 0xffffffffffff then invalid_arg "Too big pack offset" ;
  (uid lsl 48) lor offset

let run_populate _quiet rowex (uid, pack) =
  Miou_unix.run @@ fun () ->
  let rowex = Bancos.openfile ~readers:0 ~writers:2 (Fpath.to_string rowex) in
  let insert (offset, value) =
    assert (Carton.Value.kind value = `D) ;
    let str = Carton.Value.string value in
    match Tree.of_string str with
    | Ok { Tree.msgid; _ } ->
        let value = value_of_uid_and_offset uid offset in
        Some (Bancos.insert rowex (Rowex.key msgid) value)
    | Error _ ->
        Logs.warn (fun m -> m "Invalid tree at %016x" offset) ;
        None in
  let via =
    let open Flux.Flow in
    Pack.list ~kind:`D pack << filter_map insert in
  let from = Flux.Source.file ~filename:(Fpath.to_string pack) 0x7ff in
  let wait =
    let init = Fun.const ()
    and push () cmd =
      match Bancos.await cmd with
      | `Ok -> ()
      | `Duplicate key ->
          Logs.warn (fun m -> m "%s already exists" (key :> string))
      | `Found _ | `Not_found _ | `Exists _ -> ()
      | `Too_many_retries key ->
          Logs.warn (fun m -> m "Too many retries for %s" (key :> string))
    and full = Fun.const false
    and stop = Fun.id in
    Flux.Sink { init; push; full; stop } in
  let (), leftover = Flux.Stream.run ~from ~via ~into:wait in
  Option.iter Flux.Source.dispose leftover ;
  Bancos.close rowex

module Gr = Graph.Imperative.Digraph.Concrete (String)
module Traverse = Graph.Traverse.Bfs (Gr)
module Components = Graph.Components.Make (Gr)

type 'v tree = Node of 'v * 'v tree list

let rec map_tree fn (Node (v, children)) =
  Node (fn v, List.map (map_tree fn) children)

let forest graph =
  let roots = ref [] in
  let rec tree_from_root root =
    let children = ref [] in
    let fn succ = children := tree_from_root succ :: !children in
    Gr.iter_succ fn graph root ;
    Node (root, !children) in
  let fn vertex =
    Logs.debug (fun m -> m "root of %s" vertex) ;
    if Gr.in_degree graph vertex = 0
    then
      let tree = tree_from_root vertex in
      roots := tree :: !roots in
  Gr.iter_vertex fn graph ;
  !roots

let display_tree =
  Lazy.from_fun @@ fun () ->
  match Fmt.utf_8 Fmt.stdout with
  | true -> (("┌── ", "│   "), ("├── ", "│   "), ("└── ", "    "), "─┐", "·")
  | false -> ((".-- ", "|   "), ("|-- ", "|   "), ("`-- ", "    "), "-.", "x")

let rec pp_tree ?(prefix = "") ?(is_last = false) pp_elt ppf
    (Node (elt, children)) =
  let _, between, last, expand, _ = Lazy.force display_tree in
  let branch, next_prefix = if is_last then last else between in
  match children with
  | [] -> Fmt.pf ppf "%s%s%a@." prefix branch pp_elt elt
  | children ->
      Fmt.pf ppf "%s%s%a%s@." prefix branch pp_elt elt expand ;
      let prefix = prefix ^ next_prefix in
      let rec go = function
        | [] -> ()
        | [ x ] -> pp_tree ~prefix ~is_last:true pp_elt ppf x
        | x :: r ->
            pp_tree ~prefix ~is_last:false pp_elt ppf x ;
            go r in
      go children

let pp_tree pp_elt ppf (Node (elt, children) as node) =
  match children with
  | [] -> pp_tree pp_elt ppf node
  | children ->
      let top, _, _, expand, _ = Lazy.force display_tree in
      let branch, next_prefix = top in
      Fmt.pf ppf "%s%a%s@." branch pp_elt elt expand ;
      let prefix = next_prefix in
      let rec go = function
        | [] -> ()
        | [ x ] -> pp_tree ~prefix ~is_last:true pp_elt ppf x
        | x :: r ->
            pp_tree ~prefix ~is_last:false pp_elt ppf x ;
            go r in
      go children

let run_tree _quiet rowex packs =
  Miou_unix.run @@ fun () ->
  let len = List.fold_left (fun acc (uid, _) -> Int.max acc uid) 0 packs in
  let packs =
    Array.init (len + 1) @@ fun idx ->
    let fn (uid, pack) = if uid = idx then Some pack else None in
    List.find_map fn packs in
  let fn = function
    | None as none -> none
    | Some filepath ->
        let idx = Fpath.set_ext ".idx" filepath in
        let idx = Pack.index idx in
        let index (uid : Carton.Uid.t) =
          let uid = Classeur.uid_of_string_exn idx (uid :> string) in
          Carton.Local (Classeur.find_offset idx uid) in
        Some (Pack.make ~index filepath) in
  let packs = Array.map fn packs in
  let rowex = Bancos.openfile ~readers:4 ~writers:0 (Fpath.to_string rowex) in
  let graph = Gr.create () in
  let index = Art.make () in
  let fn _key value =
    let pack = value lsr 48 in
    if pack < Array.length packs
    then
      match packs.(pack) with
      | None -> ()
      | Some pack -> (
          let cursor = value land 0xffffffffffff in
          let size = Carton.size_of_offset pack ~cursor Carton.Size.zero in
          let blob = Carton.Blob.make ~size in
          let value = Carton.of_offset pack blob ~cursor in
          match Tree.of_string (Carton.Value.string value) with
          | Ok tree ->
              Art.insert index (Art.key tree.Tree.msgid) tree.Tree.uid ;
              List.iter (Gr.add_edge graph tree.Tree.msgid) tree.Tree.children ;
              Logs.debug (fun m ->
                  m "%s => %a" tree.Tree.msgid Carton.Uid.pp tree.Tree.uid)
          | Error _ -> Logs.warn (fun m -> m "Invalid tree at %016x" cursor))
  in
  let cmd = Bancos.iter ~fn rowex in
  ignore (Bancos.await cmd) ;
  let trees = forest graph in
  let fn msgid =
    match Art.find_opt index (Art.key msgid) with
    | Some uid -> `Uid uid
    | None -> `MsgID msgid in
  let trees = List.map (map_tree fn) trees in
  let pp_elt ppf = function
    | `Uid uid -> Fmt.pf ppf "[%a]" Carton.Uid.pp uid
    | `MsgID msgid -> Fmt.string ppf msgid in
  List.iter (Fmt.pr "%a@." (pp_tree pp_elt)) trees ;
  Bancos.close rowex

open Cmdliner
open Blaze_cli

let pack_with_uid =
  let parser str =
    let[@warning "-8"] (uid :: pack) = String.split_on_char ':' str in
    let pack = String.concat ":" pack in
    match (int_of_string_opt uid, Fpath.of_string pack) with
    | Some uid, Ok filepath
      when Sys.file_exists pack && Sys.is_regular_file pack ->
        Ok (uid, filepath)
    | None, _ -> error_msgf "Invalid unique PACK identifier: %S" uid
    | _, Ok filepath ->
        error_msgf "%a is not an existing and/or regular file" Fpath.pp filepath
    | _, (Error _ as err) -> err in
  let pp ppf (uid, filepath) = Fmt.pf ppf "%d:%a" uid Fpath.pp filepath in
  Arg.conv (parser, pp)

let pack =
  let doc = "The PACKv2 file with its unique identifier." in
  let open Arg in
  required
  & opt (some pack_with_uid) None
  & info [ "p"; "pack" ] ~doc ~docv:"UID:PACK"

let rowex =
  let doc = "The ROWEX file." in
  let open Arg in
  required
  & opt (some Blaze_cli.file) None
  & info [ "i"; "rowex" ] ~doc ~docv:"FILE"

let populate_term =
  let open Term in
  const run_populate $ setup_logs $ rowex $ pack

let populate_cmd =
  let doc = "A tool to populate a ROWEX file from a PACKv2 archive." in
  let man = [] in
  let info = Cmd.info "populate" ~doc ~man in
  Cmd.v info populate_term

let pack_with_uid_and_idx =
  let parser str =
    let[@warning "-8"] (uid :: pack) = String.split_on_char ':' str in
    let pack = String.concat ":" pack in
    match (int_of_string_opt uid, Fpath.of_string pack) with
    | Some uid, Ok filepath
      when Sys.file_exists pack && Sys.is_regular_file pack ->
        let idx = Fpath.set_ext ".idx" filepath in
        if
          Sys.file_exists (Fpath.to_string idx)
          && Sys.is_regular_file (Fpath.to_string idx)
        then Ok (uid, filepath)
        else
          error_msgf "Your PACKv2 file %a must be associated with an IDX file"
            Fpath.pp filepath
    | None, _ -> error_msgf "Invalid unique PACK identifier: %S" uid
    | _, Ok filepath ->
        error_msgf "%a is not an existing and/or regular file" Fpath.pp filepath
    | _, (Error _ as err) -> err in
  let pp ppf (uid, filepath) = Fmt.pf ppf "%d:%a" uid Fpath.pp filepath in
  Arg.conv (parser, pp)

let packs =
  let doc = "PACKv2 files as archives of emails." in
  let open Arg in
  value & opt_all pack_with_uid [] & info [ "p"; "pack" ] ~doc ~docv:"UID:PACK"

let tree_term =
  let open Term in
  const run_tree $ setup_logs $ rowex $ packs

let tree_cmd =
  let doc = "A tool to iter on emails from the given ROWEX file." in
  let man = [] in
  let info = Cmd.info "tree" ~doc ~man in
  Cmd.v info tree_term

let default = Term.(ret (const (`Help (`Pager, None))))

let cmd =
  let doc = "A tool to manipulate a ROWEX file." in
  let man = [] in
  Cmd.group ~default (Cmd.info "rowex" ~doc ~man) [ populate_cmd; tree_cmd ]
