(* (c) David Kaloper <david@numm.org> *)

open Lwt
open Nocrypto

module Make (Entry_points : Mirage_main.S) = struct
  module Entropy_connector = Entropy.Make(Entry_points)

  type t = { e : Entropy.t
           ; token : Entropy.token
           ; g : Rng.g }

  let attach e g =
    let `Acc acc = Rng.accumulate (Some g) in
    Entropy.add_handler e acc

  let active = ref None
  and mx = Lwt_mutex.create ()

  let initialize () =
    Lwt_mutex.with_lock mx @@ fun () ->
    let g = !Rng.generator in
    let reg e = attach e g >|= fun token -> active := Some { e; token; g; } in
    match !active with
    | Some t when t.g == g -> Lwt.return ()
    | Some t -> Entropy.remove_handler t.e t.token ; reg t.e
    | None -> Entropy_connector.connect () >>= reg

  include Rng
end
