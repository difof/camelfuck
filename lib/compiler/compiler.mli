type error =
  | UnmatchedClosingBracket of int
  | UnmatchedOpeningBracket of int

val pp_error : Format.formatter -> error -> unit
val parse_sequence : string -> Isa.intr list
val fuse_std_ops : Isa.intr list -> Isa.intr list
val optimize_patterns : Isa.intr list -> Isa.intr list
val resolve_jumps : Isa.intr list -> (Isa.t list, error) result
val full_pass : string -> (Isa.t list, error) result
