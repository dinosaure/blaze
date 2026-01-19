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
  val pp : 'a Fmt.t -> 'a t Fmt.t
end

module Semantic : sig
  type 'octet document =
    | Leaf of { mime : string; lang : Snowball.Language.t; contents : 'octet }
    | Choose of { mime : string; parts : 'octet document list }

  and 'octet t = 'octet document option

  val fold :
    ('acc -> string * Snowball.Language.t * 'a -> 'acc) -> 'acc -> 'a t -> 'acc

  val pp : 'a Fmt.t -> 'a t Fmt.t
end

type 'octet t = 'octet Skeleton.t * 'octet Semantic.t
type with_offsets = (int * int) t

val map : ('a -> 'b) -> 'a t -> 'b t

module Format : sig
  val t : string t Encore.t
end

module Parser : sig
  val t : (int * int) Skeleton.t Angstrom.t
end

type metadata = {
  message_id : Mrmime.MessageID.t option;
  in_reply_to : Mrmime.MessageID.t option;
  from : Mrmime.Mailbox.t list;
  sender : Mrmime.Mailbox.t option;
  reply_to : Mrmime.Address.t list;
  _to : Mrmime.Address.t list;
  date : (Ptime.t * Ptime.tz_offset_s) option;
  subject : Unstrctrd.t option;
}

type error = [ `Invalid_email | `No_symmetry | `Not_enough | `Msg of string ]

val of_filepath :
  ?lang:Snowball.Language.t ->
  Fpath.t ->
  (with_offsets * metadata, [> error ]) result

val to_seq :
  load:('a -> 'b) -> 'a Skeleton.t -> [ `String of string | `Value of 'b ] Seq.t

val of_string : string -> (string t, [> `Msg of string ]) result
val of_bstr : Bstr.t -> (string t, [> `Msg of string ]) result
val to_string : string t -> string

(**/**)

val output_bstr : out_channel -> Bstr.t -> unit
