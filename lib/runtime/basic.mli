type t

type error =
  | MissingOpenBracket of int
  | MissingCloseBracket of int
  | TapeError of Tape.error

val pp_error : Format.formatter -> error -> unit
val of_string : in_channel * out_channel -> string -> (t, error) result
val run : string -> (t, error) result
