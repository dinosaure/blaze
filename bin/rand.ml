let run _ seed len output =
  let g = Mirage_crypto_rng.Fortuna.create () in
  let g = Mirage_crypto_rng.(create ~g ~seed (module Fortuna)) in
  Mirage_crypto_rng.set_default_generator g ;
  let res = Mirage_crypto_rng.generate ~g len in
  match output with
  | `Base64 ->
      let b64 = Base64.encode_exn res in
      Fmt.pr "%s\n%!" b64
  | `Hex ->
      let hex = Ohex.encode res in
      Fmt.pr "%s\n%!" hex
  | `Raw -> Fmt.pr "%s" res

open Cmdliner
open Blaze_cli

let seed =
  let parser str =
    match Base64.decode ~pad:true str with
    | Ok _ as value -> value
    | Error _ as err -> err in
  let pp ppf str = Fmt.string ppf (Base64.encode_string ~pad:true str) in
  Arg.conv (parser, pp)

let seed =
  let doc = "Seed used to initialize the Fortuna random number generator." in
  Arg.(required & opt (some seed) None & info [ "seed" ] ~doc ~docv:"BASE64")

let length =
  let doc = "The length of the random value (in bytes)" in
  Arg.(required & pos 0 (some int) None & info [] ~doc ~docv:"LENGTH")

let output =
  let base64 =
    let doc = "Outputs the random value in the base64 format." in
    Arg.info [ "base64" ] ~doc in
  let hex =
    let doc = "Outputs the random value in the hex format." in
    Arg.info [ "hex" ] ~doc in
  let raw =
    let doc = "Outputs the random value as it is." in
    Arg.info [ "raw" ] ~doc in
  Arg.(value & vflag `Base64 [ (`Base64, base64); (`Hex, hex); (`Raw, raw) ])

let cmd =
  let doc = "Generate a random value from a seed with the fortuna algorithm." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) generates a random value from a seed with the $(b,fortuna) \
         algorithm. The user can set the output of the value (base64 or hex)";
    ] in
  let open Term in
  let info = Cmd.info "rand" ~doc ~man in
  let term = const run $ setup_logs $ seed $ length $ output in
  Cmd.v info term
