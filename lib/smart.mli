type error =
  [ Protocol.error
  | `No_branch
  | `Invalid_version of string
  | `Invalid_negotiation
  | `No_side_band
  | `Err of string ]

val pp_error : error Fmt.t

type refs = {
  refs : (string * string) list;
  head : string;
  head_symref : string option;
}

type advertisement =
  | V1 of { refs : refs; capabilities : string list }
  | V2 of { capabilities : string list }

val advertisement : Protocol.ctx -> (advertisement, [> error ]) Protocol.t
val ls_refs : Protocol.ctx -> (refs, [> error ]) Protocol.t

val fetch_v1 :
  capabilities:string list ->
  want:string ->
  (string, 'r) Flux.Bqueue.t ->
  Protocol.ctx ->
  (bool, [> error ]) Protocol.t

val fetch_v2 :
  want:string ->
  (string, 'r) Flux.Bqueue.t ->
  Protocol.ctx ->
  (bool, [> error ]) Protocol.t

val clone :
  protocol:[> `Git of string ] ->
  Protocol.ctx ->
  (string, 'r) Flux.Bqueue.t ->
  (bool, [> error ]) Protocol.t
