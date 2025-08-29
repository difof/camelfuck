type intermediate_instr =
  | Instr of Instruction.t
  | OpenLoop
  | CloseLoop

type intermediate_instr_w_offset = IntInstrWOffset of intermediate_instr * int

type error =
  | UnmatchedClosingBracket of int
  | UnmatchedOpeningBracket of int
  | EncodingError of Instruction.error

val pp_error : Format.formatter -> error -> unit
val pp_intermediate_instr : Format.formatter -> intermediate_instr -> unit

val pp_intermediate_instr_w_offset
  :  Format.formatter
  -> intermediate_instr_w_offset
  -> unit

val parse_sequence : string -> intermediate_instr list
val fuse_std_ops : intermediate_instr list -> intermediate_instr list
val optimize_patterns : intermediate_instr list -> intermediate_instr list
val optimize_single_ops : intermediate_instr list -> intermediate_instr list
val bind_instruction_offsets : intermediate_instr list -> intermediate_instr_w_offset list
val resolve_jumps : intermediate_instr_w_offset list -> (Instruction.t list, error) result
val encode_to_bytes : Instruction.t list -> (bytes list, error) result
val combine_instruction_bytes : bytes list -> bytes
val compile : string -> (bytes, error) result
