type state =
  | Continue of Mrmime.Field.field list * Mrmime.Hd.decoder
  | Done of string * Mrmime.Header.t
  | Fail

type error = [ `Invalid_email | `Not_enough ]

let parser p =
  let rec go acc decoder =
    match Mrmime.Hd.decode decoder with
    | `Malformed _err -> Fail
    | `Field field -> go (Mrmime.Location.prj field :: acc) decoder
    | `End prelude -> Done (prelude, Mrmime.Header.of_list (List.rev acc))
    | `Await -> Continue (acc, decoder) in
  let init () = Continue ([], Mrmime.Hd.decoder p)
  and push state str =
    match state with
    | Continue (acc, decoder) ->
        Mrmime.Hd.src decoder str 0 (String.length str) ;
        go acc decoder
    | (Done _ | Fail) as state -> state
  and full = function Continue _ -> false | Done _ | Fail -> true
  and stop = function
    | Done (prelude, hdrs) -> Ok (prelude, hdrs)
    | Fail -> Error `Invalid_email
    | Continue (acc, decoder) -> (
        Mrmime.Hd.src decoder String.empty 0 0 ;
        match go acc decoder with
        | Done (prelude, hdrs) -> Ok (prelude, hdrs)
        | Fail -> Error `Invalid_email
        | Continue _ -> Error `Not_enough) in
  Flux.Sink { init; push; full; stop }
