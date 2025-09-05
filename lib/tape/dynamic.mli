type t

val create : ?max_size:int -> int -> t
val move : t -> int -> unit
val scan_to_zero : t -> int -> unit
val get : t -> int
val set : t -> int -> unit
val add : t -> int -> unit
val set_at_offset : t -> int -> int -> unit
val add_at_offset : t -> int -> int -> unit
val mulclear : t -> int -> unit
val trailadd : t -> (int * int) list -> unit
val multransfer : t -> (int * int) list -> unit
val transfer : t -> int -> unit
val len : t -> int
val bias : t -> int
val logical_pos : t -> int
val physical_pos : t -> int
val blit_out : t -> int array -> int -> unit
val blit_in : t -> int array -> int -> unit
