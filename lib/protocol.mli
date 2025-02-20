module Decoder : sig
  type t = { buffer : bytes; mutable pos : int; mutable max : int }

  val make : int -> t

  type ('v, 'err) state =
    | Done of 'v
    | Read of {
        buffer : bytes;
        off : int;
        len : int;
        continue : ('v, 'err) continue;
      }
    | Error of 'err info

  and ('v, 'err) continue = [ `End | `Len of int ] -> ('v, 'err) state
  and 'err info = { error : 'err; buffer : bytes; committed : int }

  type error = [ `End_of_input | `Not_enough_space | `Expected_eol ]

  val return : 'v -> t -> ('v, 'err) state
  val prompt : (t -> ('v, ([> error ] as 'err)) state) -> t -> ('v, 'err) state
  val peek_while_eol : t -> bytes * int * int
end

module Encoder : sig
  type t

  val make : int -> t

  type 'err state =
    | Write of {
        buffer : string;
        off : int;
        len : int;
        continue : 'err continue;
      }
    | Error of 'err
    | Done

  and 'err continue = int -> 'err state

  type error = [ `Not_enough_space ]

  val flush : (t -> ([> error ] as 'err) state) -> t -> 'err state
  val write : string -> t -> unit
  val blit : string -> off:int -> len:int -> t -> unit
  val safe : (t -> ([> error ] as 'err) state) -> t -> 'err state
end

type (+'a, 'err) t =
  | Read of {
      buffer : bytes;
      off : int;
      len : int;
      k : [ `End | `Len of int ] -> ('a, 'err) t;
    }
  | Write of { buffer : string; off : int; len : int; k : int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err

val reword_error : ('err0 -> 'err1) -> ('a, 'err0) t -> ('a, 'err1) t
val bind : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
val return : 'a -> ('a, 'err) t
val error : 'err -> ('a, 'err) t

type ctx
type error = [ Decoder.error | Encoder.error ]

val pp_error : error Fmt.t
val ctx : unit -> ctx
val encode : ctx -> string -> (unit, [> Encoder.error ]) t
val decode : ctx -> (string, [> Decoder.error ]) t
