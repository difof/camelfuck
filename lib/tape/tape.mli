(* Bi-directional size capped linear tape memory of bytes *)

type t

type bias_offset =
  | Start
  | Middle

type error =
  | MaxAllocationReached of int * int
  | BlitOutOfBounds of int * int * int
  | BlitNegativeLength of int

exception TapeExn of error

(* Prints the Tape exceptions *)
val pp_error : Format.formatter -> error -> unit

(* Create a new Tape. The initial size will have minimum size of 256 *)
val create : ?bias_offset:bias_offset -> ?max_size:int -> int -> t

(*
   Move the Tape pointer by given offset. It can be negative.
    Throws `MaxAllocationReached`
*)
val move_exn : t -> int -> unit
val move : t -> int -> (unit, error) result

(* Get the value of current Tape pointer *)
val get : t -> int

(* Set the value of current Tape pointer masked by 0xFF *)
val set : t -> int -> unit

(* Add to current cell (masked) *)
val add : t -> int -> unit

(* Write at logical offset from current pointer without moving it (masked) *)
val set_at_offset_exn : t -> int -> int -> unit
val set_at_offset : t -> int -> int -> (unit, error) result

(* Add a value at logical offset from current pointer without moving it (masked) *)
val add_at_offset_exn : t -> int -> int -> unit
val add_at_offset : t -> int -> int -> (unit, error) result

(* Get the total length of the Tape *)
val len : t -> int

(* Get the center bias of the Tape, used for negative/positive indexing *)
val bias : t -> int

(* Get the logical Tape pointer position, it can be negative *)
val logical_pos : t -> int

(* Get the computed physical Tape pointer position in the buffer *)
val physical_pos : t -> int

(* Get the max size of the Tape. It cannot grow larger than this *)
val max_size : t -> int

(*
   Get a slice of the Tape. 
    If given size is larger than Tape's current position + len, it will throw `BlitOutOfBounds`
*)
val blit_out_exn : t -> bytes -> int -> unit
val blit_out : t -> bytes -> int -> (unit, error) result

(*
   Copy the given buffer's contents into Tape, starting at current tape pointer. 
    If Tape limits are reached, it won't grow and throw `BlitOutOfBounds` instead.
*)
val blit_in_exn : t -> bytes -> int -> unit
val blit_in : t -> bytes -> int -> (unit, error) result

(*
   Copy the given buffer's contents into Tape, starting at current tape pointer. 
    It will grow the Tape as needed.
*)
val blit_in_ensure_exn : t -> bytes -> int -> unit
val blit_in_ensure : t -> bytes -> int -> (unit, error) result
