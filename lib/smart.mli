type error =
  [ Protocol.error
  | `No_branch
  | `Invalid_version of string
  | `Invalid_negotiation ]

val pp_error : error Fmt.t

val clone :
  protocol:[> `Git of string ] ->
  Protocol.ctx ->
  (string, 'r) Flux.Bqueue.t ->
  (bool, [> error ]) Protocol.t
