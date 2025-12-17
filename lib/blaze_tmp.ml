let kprng = Domain.DLS.new_key Random.State.make_self_init

let kdirname =
  Domain.DLS.new_key ~split_from_parent:Fun.id @@ fun () ->
  match Sys.getenv "TMPDIR" with
  | value -> value
  | exception Not_found -> "/tmp"

let set_temp_dirname v = Domain.DLS.set kdirname v
let get_temp_dirname () = Domain.DLS.get kdirname
let to_delete = ref []
let register_to_delete filepath = to_delete := filepath :: !to_delete
let () = at_exit @@ fun () -> List.iter Unix.unlink !to_delete

let temp_filename fmt =
  let g = Domain.DLS.get kprng in
  let v = Random.State.bits g land 0xffffff in
  Fmt.kstr (Filename.concat (get_temp_dirname ())) fmt v

let temp_filepath ?(clean = true) fmt =
  let flags = [ Open_wronly; Open_creat; Open_excl ] in
  let rec try_filepath counter =
    let filepath = temp_filename fmt in
    match close_in (open_in_gen flags 0o600 filepath) with
    | () ->
        if clean then register_to_delete filepath ;
        filepath
    | exception Sys_error _ ->
        if counter >= 20
        then invalid_arg "Impossible to create a temporary file."
        else try_filepath (succ counter) in
  try_filepath 0
