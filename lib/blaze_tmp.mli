val set_temp_dirname : string -> unit
val get_temp_dirname : unit -> string

val temp_filepath :
  ?clean:bool ->
  (int -> string, Format.formatter, unit, string) format4 ->
  string
