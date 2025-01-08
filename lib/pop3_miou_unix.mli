module Stream : sig
  type 'a t

  val make : int -> 'a t
  val put : 'a t -> 'a option -> unit
  val get : 'a t -> 'a option
  val to_seq : 'a t -> 'a Seq.t
end

type error = [ Pop3.error | `Msg of string ]

val pp_error : error Fmt.t

val fetch :
  ?authentication:string * string ->
  ?cfg:Tls.Config.client ->
  ?authenticator:X509.Authenticator.t ->
  ?ports:int list ->
  server:string ->
  filter:(Pop3.Uid.t list -> Pop3.Uid.t list) ->
  Happy_eyeballs_miou_unix.t ->
  (Pop3.Uid.t * string Stream.t) Stream.t ->
  (unit, [> error ]) result
