type intermediate_instr =
  | Instr of Instruction.t
  | OpenLoop
  | CloseLoop

val parse_sequence : string -> intermediate_instr list
val resolve_jumps : intermediate_instr list -> Instruction.t list
val optimize_instructions : Instruction.t list -> Instruction.t list
val pattern_optimize : Instruction.t list -> Instruction.t list
val encode_to_bytes : Instruction.t list -> bytes list
val combine_instruction_bytes : bytes list -> bytes
val compile : string -> bytes
