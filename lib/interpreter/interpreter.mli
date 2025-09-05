type error =
  | InvalidInstruction of int
  | BytecodeOutOfBounds of int
  | JumpOutOfBounds of int
  | TapeError of Tape.TapeError.t
  | Exception of exn

exception VMExn of error

val pp_error : Format.formatter -> error -> unit

module Make (Tape_intf : Tape.S) : sig
  type t

  val create : ?io:in_channel * out_channel -> Tape_intf.t -> Isa.t list -> t
  val run : t -> (unit, error) result
end
