type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Skeleton : sig
  type transport_padding = string

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
end

module Semantic : sig
  type 'octet document =
    | Leaf of { mime : string; lang : Snowball.Language.t; contents : 'octet }
    | Choose of { mime : string; parts : 'octet document list }

  and 'octet t = 'octet document option
end

type 'octet t = 'octet Skeleton.t * 'octet Semantic.t

module Format : sig
  val t : string t Encore.t
end

module Parser : sig
  val t : (int * int) Skeleton.t Angstrom.t
end

val of_filename :
  ?lang:Snowball.Language.t ->
  Fpath.t ->
  ((int * int) t, [> `Invalid | `No_symmetry | `Not_enough ]) result

val to_seq :
  load:('a -> 'b) -> 'a Skeleton.t -> [ `String of string | `Value of 'b ] Seq.t

val to_output_channel_from_filename :
  Fpath.t -> (int * int) Skeleton.t -> out_channel -> unit

val of_string : string -> (string t, [> `Msg of string ]) result
val of_bigstring : bigstring -> (string t, [> `Msg of string ]) result
val to_string : string t -> string

(**/**)

val output_bigstring : out_channel -> bigstring -> unit
