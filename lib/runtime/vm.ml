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

exception VMExn of string

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

let[@inline] ensure_can_read t bytes =
  let last = t.pc + bytes in
  if last >= t.code_length then raise (VMExn "bytecode out of bounds")
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
    while true do
      ()
    done;
    t
  | '\x01' ->
    (* AddN imm8: size 2 *)
    let imm = read_imm_i8 t in
    Tape.set t.memory (Tape.get t.memory + imm);
    t.pc <- t.pc + 2;
    t
  | '\x02' ->
    (* MoveN imm8: size 2 *)
    let imm = read_imm_i8 t in
    Tape.move_exn t.memory imm;
    t.pc <- t.pc + 2;
    t
  | '\x03' ->
    (* Jz imm32: size 5, relative to current pc *)
    let rel = read_imm_i32 t in
    if Tape.get t.memory = 0
    then (
      let new_pc = t.pc + rel in
      if new_pc < 0 || new_pc > t.code_length then raise (VMExn "jump out of bounds");
      t.pc <- new_pc;
      t)
    else (
      t.pc <- t.pc + 5;
      t)
  | '\x04' ->
    (* Jnz imm32: size 5, relative to current pc *)
    let rel = read_imm_i32 t in
    if Tape.get t.memory <> 0
    then (
      let new_pc = t.pc + rel in
      if new_pc < 0 || new_pc > t.code_length then raise (VMExn "jump out of bounds");
      t.pc <- new_pc;
      t)
    else (
      t.pc <- t.pc + 5;
      t)
  | '\x05' ->
    (* In *)
    let v = In_channel.input_char t.input |> Option.value ~default:'\000' |> Char.code in
    Tape.set t.memory v;
    t.pc <- t.pc + 1;
    t
  | '\x06' ->
    (* Out *)
    Tape.get t.memory |> Char.chr |> Out_channel.output_char t.output;
    Out_channel.flush t.output;
    t.pc <- t.pc + 1;
    t
  | '\x07' ->
    (* Call: runtime extension placeholder - no-op for now *)
    t.pc <- t.pc + 1;
    t
  | '\x08' ->
    (* SetZero *)
    Tape.set t.memory 0;
    t.pc <- t.pc + 1;
    t
  | '\x09' ->
    (* TransferR: move current cell value to right cell, zero current *)
    let v = Tape.get t.memory in
    if v <> 0
    then (
      Tape.set t.memory 0;
      Tape.move_exn t.memory 1;
      Tape.set t.memory (Tape.get t.memory + v);
      Tape.move_exn t.memory (-1));
    t.pc <- t.pc + 1;
    t
  | '\x0A' ->
    (* Add1 *)
    Tape.set t.memory (Tape.get t.memory + 1);
    t.pc <- t.pc + 1;
    t
  | '\x0B' ->
    (* Sub1 *)
    Tape.set t.memory (Tape.get t.memory - 1);
    t.pc <- t.pc + 1;
    t
  | '\x0C' ->
    (* Move1R *)
    Tape.move_exn t.memory 1;
    t.pc <- t.pc + 1;
    t
  | '\x0D' ->
    (* Move1L *)
    Tape.move_exn t.memory (-1);
    t.pc <- t.pc + 1;
    t
  | _ ->
    (* Unknown opcode: treat as no-op advance *)
    t.pc <- t.pc + 1;
    t
;;

let rec run t =
  if t.pc >= t.code_length then t else run (Bytes.unsafe_get t.code t.pc |> exec_instr t)
;;
