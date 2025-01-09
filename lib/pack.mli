type src = Mail of string | Body of Fpath.t * int * int

val sha1 : Carton.First_pass.digest
val mail_identify : Digestif.SHA1.ctx Carton.First_pass.identify

val config :
  ?pagesize:int ->
  ?cachesize:int ->
  ?threads:int ->
  ?on_entry:(max:int -> Carton_miou_unix.entry -> unit) ->
  ?on_object:(cursor:int -> Carton.Value.t -> Carton.Uid.t -> unit) ->
  unit ->
  Carton_miou_unix.config

val filename_to_email : Fpath.t -> Fpath.t * (int * int) Email.t

val email_to_entries :
  Fpath.t * (int * int) Email.t -> src Cartonnage.Entry.t list

val uid_of_value : Carton.Value.t -> Carton.Uid.t

val delta :
  load:(Carton.Uid.t -> 'meta -> Carton.Value.t) ->
  'meta Cartonnage.Entry.t Seq.t ->
  'meta Cartonnage.Target.t Seq.t

val to_pack :
  ?with_header:int ->
  ?with_signature:Digestif.SHA1.ctx ->
  load:(Carton.Uid.t -> 'meta -> Carton.Value.t) ->
  'meta Cartonnage.Target.t Seq.t ->
  string Seq.t

val verify_from_pack :
  cfg:Carton_miou_unix.config -> Fpath.t -> Carton.status array * string

val verify_from_idx :
  cfg:Carton_miou_unix.config -> Fpath.t -> Carton.status array * string

val make :
  ?index:(Carton.Uid.t -> int) ->
  Fpath.t ->
  Carton_miou_unix.file_descr Carton.t

val index : Fpath.t -> Carton_miou_unix.file_descr Classeur.t
