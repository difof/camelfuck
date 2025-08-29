type t

type error =
  | InvalidInstruction of char
  | BytecodeOutOfBounds of int
  | JumpOutOfBounds of int
  | TapeError of Tape.error
  | Exception of exn

val pp_error : Format.formatter -> error -> unit

exception VMExn of error

val create : ?io:in_channel * out_channel -> ?memory:Tape.t -> bytes -> t
val run : t -> (t, error) result
