module Uid : sig
  type t

  val equal_to_string : t -> string -> bool
  val pp : t Fmt.t
  val to_string : t -> string
end

type choose = Uid.t list -> Uid.t list
type emitters = uid:Uid.t -> string option -> unit
type error = [ Protocol.error | `POP3 of string ]

val pp_error : error Fmt.t

val fetch :
  ?authentication:string * string ->
  choose:choose ->
  emitter_of:emitters ->
  Protocol.ctx ->
  (unit, [> error ]) Protocol.t
