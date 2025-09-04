type t

type error =
  | InvalidInstruction of int
  | BytecodeOutOfBounds of int
  | JumpOutOfBounds of int
  | TapeError of Tape.error
  | Exception of exn

exception VMExn of error

val pp_error : Format.formatter -> error -> unit
val create : ?io:in_channel * out_channel -> ?memory:Tape.t -> Isa.t list -> t
val run : t -> (unit, error) result
