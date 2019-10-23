module Make (Entry_points : Mirage_main.S) : sig
  include Mirage_random.C
  val initialize : unit -> unit Lwt.t
end
