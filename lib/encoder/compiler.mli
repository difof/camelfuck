type intermediate_instr =
  | Instr of Instruction.t
  | OpenLoop
  | CloseLoop

type error =
  | UnmatchedClosingBracket
  | UnmatchedOpeningBracket
  | EncodingError of Instruction.error

val parse_sequence : string -> intermediate_instr list
val optimize_instructions : intermediate_instr list -> intermediate_instr list
val pattern_optimize : intermediate_instr list -> intermediate_instr list
val resolve_jumps : intermediate_instr list -> (Instruction.t list, error) result
val encode_to_bytes : Instruction.t list -> (bytes list, error) result
val combine_instruction_bytes : bytes list -> bytes
val compile : string -> (bytes, error) result
