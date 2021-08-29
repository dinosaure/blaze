let blit0 src src_off dst dst_off len =
  let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
  Cstruct.blit src src_off dst 0 len

let blit1 src src_off dst dst_off len =
  let src = Cstruct.of_bigarray ~off:src_off ~len src in
  Cstruct.blit src 0 dst dst_off len

module type FLOW = sig
  type t

  type error

  val read : t -> ([ `Data of Cstruct.t | `Eof ], error) result

  val write : t -> Cstruct.t -> (unit, error) result

  val writev : t -> Cstruct.t list -> (unit, error) result
end

open Rresult

module Make (Flow : FLOW) = struct
  type t = {
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    flow : Flow.t;
  }

  type error = Flow.error

  let make flow = { flow; queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char }

  let recv flow payload =
    if Ke.Rke.is_empty flow.queue
    then (
      Flow.read flow.flow |> R.reword_error (fun err -> `Error err) >>= function
      | `Eof -> R.ok `End_of_flow
      | `Data res ->
          Ke.Rke.N.push flow.queue ~blit:blit0 ~length:Cstruct.length res ;
          let len = min (Cstruct.length payload) (Ke.Rke.length flow.queue) in
          Ke.Rke.N.keep_exn flow.queue ~blit:blit1 ~length:Cstruct.length ~off:0
            ~len payload ;
          Ke.Rke.N.shift_exn flow.queue len ;
          R.ok (`Input len))
    else
      let len = min (Cstruct.length payload) (Ke.Rke.length flow.queue) in
      Ke.Rke.N.keep_exn flow.queue ~blit:blit1 ~length:Cstruct.length payload ;
      Ke.Rke.N.shift_exn flow.queue len ;
      R.ok (`Input len)

  let send flow payload =
    Flow.write flow.flow payload |> function
    | Error err -> R.error err
    | Ok () -> R.ok (Cstruct.length payload)
end
