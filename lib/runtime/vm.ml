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
  | InvalidInstruction of char
  | BytecodeOutOfBounds of int
  | JumpOutOfBounds of int
  | TapeError of Tape.error
  | Exception of exn

let pp_error fmt = function
  | InvalidInstruction op -> Format.fprintf fmt "Invalid instruction: %x" (Char.code op)
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

let[@inline] op_scan ?(stride = 1) t =
  while Tape.get t.memory <> 0 do
    Tape.move_exn t.memory stride
  done;
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

let[@inline] ensure_i8_sign v = if v >= 128 then v - 256 else v

let[@inline] read_imm_i8 t =
  ensure_can_read t 1;
  ensure_i8_sign (Bytes.unsafe_get t.code (t.pc + 1) |> Char.code)
;;

let[@inline] read_imm_i8_2 t =
  ensure_can_read t 2;
  let b1 = Bytes.unsafe_get t.code (t.pc + 1) |> Char.code in
  let b2 = Bytes.unsafe_get t.code (t.pc + 2) |> Char.code in
  ensure_i8_sign b1, ensure_i8_sign b2
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
    (* AddN imm8 *)
    Tape.set t.memory (Tape.get t.memory + read_imm_i8 t);
    advance t ~n:2
  | '\x02' ->
    (* MoveN imm8 *)
    Tape.move_exn t.memory (read_imm_i8 t);
    advance t ~n:2
  | '\x03' ->
    (* Jz imm32 *)
    if Tape.get t.memory = 0
    then (
      let rel = read_imm_i32 t in
      let new_pc = t.pc + rel in
      ensure_pc_bounds t new_pc |> advance ~replace:new_pc)
    else advance t ~n:5
  | '\x04' ->
    (* Jnz imm32 *)
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
    (* Transfer1R *)
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
    (* Transfer1L *)
    op_transfer ~pos:(-1) t |> advance
  | '\x0F' ->
    (* TransferN imm8 *)
    let n = read_imm_i8 t in
    op_transfer ~pos:n t |> advance ~n:2
  | '\x10' ->
    (* Scan1R *)
    op_scan t |> advance
  | '\x11' ->
    (* Scan1L *)
    op_scan ~stride:(-1) t |> advance
  | '\x12' ->
    (* ScanN imm8 *)
    let n = read_imm_i8 t in
    op_scan ~stride:n t |> advance ~n:2
  | '\x13' ->
    (* AddAt imm8 imm8 *)
    let delta, n = read_imm_i8_2 t in
    Tape.move_exn t.memory delta;
    Tape.set t.memory (Tape.get t.memory + n);
    Tape.move_exn t.memory (-delta);
    advance t ~n:3
  | op -> raise (VMExn (InvalidInstruction op))
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
