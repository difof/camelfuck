type intermediate_instr =
  | Instr of Instruction.t
  | OpenLoop
  | CloseLoop

val parse_sequence : string -> int -> intermediate_instr list -> intermediate_instr list
val resolve_jumps : intermediate_instr list -> Instruction.t list
val compile : string -> bytes
