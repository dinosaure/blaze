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
  (Pop3.Uid.t * string Flux.Bqueue.c) Flux.Bqueue.c ->
  (unit, [> error ]) result
