type transport_padding = string

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'octet body = Multipart of 'octet multipart | Single of 'octet option
and 'octet part = { headers : 'octet; body : 'octet body }

and 'octet multipart = {
  preamble : string;
  epilogue : string * transport_padding;
  boundary : string;
  parts : (transport_padding * 'octet part) list;
}

val map : ('a -> 'b) -> 'a part -> 'b part
val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a part -> 'acc

module Format : sig
  val t : string part Encore.t
end

module Parser : sig
  val t : (int * int) part Angstrom.t
end

val of_filename : Fpath.t -> ((int * int) part, [> `Msg of string ]) result

val to_seq :
  load:('a -> 'b) -> 'a part -> [ `String of string | `Value of 'b ] Seq.t

val to_output_channel_from_filename :
  Fpath.t -> (int * int) part -> out_channel -> unit

val of_string : string -> (string part, [> `Msg of string ]) result
val of_bigstring : bigstring -> (string part, [> `Msg of string ]) result
val to_string : string part -> string

(**/**)

val output_bigstring : out_channel -> bigstring -> unit
