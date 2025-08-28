open Stdlib
open Stdio

type t =
  { code : bytes
  ; code_length : int
  ; memory : Tape.t
  ; input : In_channel.t
  ; output : Out_channel.t
  ; mutable pc : int
  }

type error =
  | BytecodeOutOfBounds of int
  | JumpOutOfBounds of int
  | TapeError of Tape.error
  | Exception of exn

let pp_error fmt = function
  | BytecodeOutOfBounds pos ->
    Format.fprintf fmt "Bytecode out of bounds at position %d" pos
  | JumpOutOfBounds pos -> Format.fprintf fmt "Jump out of bounds to position %d" pos
  | TapeError err -> Tape.pp_error fmt err
  | Exception ex -> Format.fprintf fmt "%s" (Printexc.to_string ex)
;;

exception VMExn of error

let create
      ?(io = In_channel.stdin, Out_channel.stdout)
      ?(memory = Tape.create 256)
      program
  =
  { code = program
  ; code_length = Bytes.length program
  ; memory
  ; input = fst io
  ; output = snd io
  ; pc = 0
  }
;;

let hang t =
  while true do
    ()
  done;
  t
;;

let[@inline] op_transfer ?(pos = 1) t =
  let v = Tape.get t.memory in
  if v <> 0
  then (
    Tape.set t.memory 0;
    Tape.move_exn t.memory pos;
    Tape.set t.memory (Tape.get t.memory + v);
    Tape.move_exn t.memory (-pos));
  t
;;

let[@inline] advance ?(replace = -1) ?(n = 1) t =
  if replace >= 0 then t.pc <- replace else t.pc <- t.pc + n;
  t
;;

let[@inline] ensure_can_read t bytes =
  let last = t.pc + bytes in
  if last >= t.code_length then raise (VMExn (BytecodeOutOfBounds last))
;;

let[@inline] ensure_pc_bounds t pos =
  if pos < 0 || pos > t.code_length then raise (VMExn (JumpOutOfBounds pos)) else t
;;

let[@inline] read_imm_i8 t =
  ensure_can_read t 1;
  let c = Bytes.unsafe_get t.code (t.pc + 1) |> Char.code in
  if c >= 128 then c - 256 else c
;;

let[@inline] read_imm_i32 t =
  ensure_can_read t 4;
  Bytes.get_int32_le t.code (t.pc + 1) |> Int32.to_int
;;

let exec_instr t = function
  | '\x00' ->
    (* Hang *)
    hang t
  | '\x01' ->
    (* AddN imm8: size 2 *)
    Tape.set t.memory (Tape.get t.memory + read_imm_i8 t);
    advance t ~n:2
  | '\x02' ->
    (* MoveN imm8: size 2 *)
    Tape.move_exn t.memory (read_imm_i8 t);
    advance t ~n:2
  | '\x03' ->
    (* Jz imm32: size 5, relative to current pc *)
    if Tape.get t.memory = 0
    then (
      let rel = read_imm_i32 t in
      let new_pc = t.pc + rel in
      ensure_pc_bounds t new_pc |> advance ~replace:new_pc)
    else advance t ~n:5
  | '\x04' ->
    (* Jnz imm32: size 5, relative to current pc *)
    if Tape.get t.memory <> 0
    then (
      let rel = read_imm_i32 t in
      let new_pc = t.pc + rel in
      ensure_pc_bounds t new_pc |> advance ~replace:new_pc)
    else advance t ~n:5
  | '\x05' ->
    (* In *)
    let v = In_channel.input_char t.input |> Option.value ~default:'\000' |> Char.code in
    Tape.set t.memory v;
    advance t
  | '\x06' ->
    (* Out *)
    Tape.get t.memory |> Char.chr |> Out_channel.output_char t.output;
    Out_channel.flush t.output;
    advance t
  | '\x07' ->
    (* Call: runtime extension placeholder - no-op for now *)
    advance t
  | '\x08' ->
    (* SetZero *)
    Tape.set t.memory 0;
    advance t
  | '\x09' ->
    (* TransferR: move current cell value to right cell, zero current *)
    op_transfer t |> advance
  | '\x0A' ->
    (* Add1 *)
    Tape.set t.memory (Tape.get t.memory + 1);
    advance t
  | '\x0B' ->
    (* Sub1 *)
    Tape.set t.memory (Tape.get t.memory - 1);
    advance t
  | '\x0C' ->
    (* Move1R *)
    Tape.move_exn t.memory 1;
    advance t
  | '\x0D' ->
    (* Move1L *)
    Tape.move_exn t.memory (-1);
    advance t
  | '\x0E' ->
    (* TransferL: move current cell value to left cell, zero current *)
    op_transfer ~pos:(-1) t |> advance
  | _ ->
    (* Unknown opcode: treat as no-op advance *)
    advance t
;;

let rec run_exn t =
  if t.pc >= t.code_length
  then t
  else run_exn (Bytes.unsafe_get t.code t.pc |> exec_instr t)
;;

let run t =
  try Ok (run_exn t) with
  | VMExn err -> Error err
  | Tape.TapeExn err -> Error (TapeError err)
;;
