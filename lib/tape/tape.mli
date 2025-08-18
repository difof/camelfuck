module type S = sig
  type t

  val create : unit -> t
  val get : t -> int
  val set : t -> int -> unit
  val pos : t -> int
  val move : t -> int -> unit
end

module Hashtape : S
module Arraytape : S
