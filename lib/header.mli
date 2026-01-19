open Mrmime

type error = [ `Invalid_email | `Not_enough ]

val parser :
  Field.witness Field_name.Map.t ->
  (string, (string * Header.t, [> error ]) result) Flux.sink
