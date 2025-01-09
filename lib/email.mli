type transport_padding = string

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'octet body =
  | Multipart of 'octet multipart
  | Single of 'octet option
  | Message of 'octet t

and 'octet part = { headers : 'octet; body : 'octet body }

and 'octet multipart = {
  preamble : string;
  epilogue : string * transport_padding;
  boundary : string;
  parts : (transport_padding * 'octet part) list;
}

and 'octet t = 'octet part

val map : ('a -> 'b) -> 'a t -> 'b t
val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

module Format : sig
  val t : string t Encore.t
end

module Parser : sig
  val t : (int * int) t Angstrom.t
end

val of_filename : Fpath.t -> ((int * int) t, [> `Msg of string ]) result

val to_seq :
  load:('a -> 'b) -> 'a t -> [ `String of string | `Value of 'b ] Seq.t

val to_output_channel_from_filename :
  Fpath.t -> (int * int) t -> out_channel -> unit

val of_string : string -> (string t, [> `Msg of string ]) result
val of_bigstring : bigstring -> (string t, [> `Msg of string ]) result
val to_string : string t -> string

(**/**)

val output_bigstring : out_channel -> bigstring -> unit
