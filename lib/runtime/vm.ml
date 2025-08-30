open Core
open Stdlib
open Stdio
open Encoder.Instruction

type t =
  { instrs : Encoder.Instruction.t array
  ; instr_count : int
  ; jump_targets : int array
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

let[@inline] ensure_bounds code code_length pos needed =
  let last = pos + needed in
  if last >= code_length then raise (VMExn (BytecodeOutOfBounds last))
;;

let decode_program program =
  let code = program in
  let code_length = Bytes.length code in
  let[@inline] i8_to_int v = if v >= 128 then v - 256 else v in
  let[@inline] read_i8 off = Bytes.unsafe_get code off |> Char.code |> i8_to_int in
  let[@inline] read_i32 off = Bytes.get_int32_le code off |> Int32.to_int in
  let[@inline] rec read_pairs acc idx remaining =
    if remaining = 0
    then List.rev acc
    else (
      let d = Bytes.unsafe_get code idx |> Char.code |> i8_to_int in
      let c = Bytes.unsafe_get code (idx + 1) |> Char.code |> i8_to_int in
      read_pairs ((d, c) :: acc) (idx + 2) (remaining - 1))
  in
  let rec loop pc instrs_rev starts_rev =
    if pc >= code_length
    then (
      let instrs = Array.of_list (List.rev instrs_rev) in
      let starts = Array.of_list (List.rev starts_rev) in
      let byte_to_index = Array.make (code_length + 1) (-1) in
      Array.iteri
        (fun i start ->
           if start >= 0 && start <= code_length then byte_to_index.(start) <- i)
        starts;
      let jump_targets = Array.make (Array.length instrs) (-1) in
      Array.iteri
        (fun i instr ->
           match instr with
           | Jz rel | Jnz rel ->
             let start_pc = starts.(i) in
             let target_pc = start_pc + rel in
             let target_idx =
               if target_pc >= 0 && target_pc <= code_length
               then byte_to_index.(target_pc)
               else -1
             in
             jump_targets.(i) <- target_idx
           | _ -> ())
        instrs;
      instrs, jump_targets)
    else (
      let op = Bytes.unsafe_get code pc in
      match op with
      | '\x00' -> loop (pc + 1) (Hang :: instrs_rev) (pc :: starts_rev)
      | '\x01' ->
        ensure_bounds code code_length pc 1;
        let n = read_i8 (pc + 1) in
        loop (pc + 2) (AddN n :: instrs_rev) (pc :: starts_rev)
      | '\x02' ->
        ensure_bounds code code_length pc 1;
        let n = read_i8 (pc + 1) in
        loop (pc + 2) (MoveN n :: instrs_rev) (pc :: starts_rev)
      | '\x03' ->
        ensure_bounds code code_length pc 4;
        let rel = read_i32 (pc + 1) in
        loop (pc + 5) (Jz rel :: instrs_rev) (pc :: starts_rev)
      | '\x04' ->
        ensure_bounds code code_length pc 4;
        let rel = read_i32 (pc + 1) in
        loop (pc + 5) (Jnz rel :: instrs_rev) (pc :: starts_rev)
      | '\x05' -> loop (pc + 1) (In :: instrs_rev) (pc :: starts_rev)
      | '\x06' -> loop (pc + 1) (Out :: instrs_rev) (pc :: starts_rev)
      | '\x07' -> loop (pc + 1) (Call :: instrs_rev) (pc :: starts_rev)
      | '\x08' -> loop (pc + 1) (SetZero :: instrs_rev) (pc :: starts_rev)
      | '\x09' -> loop (pc + 1) (Transfer1R :: instrs_rev) (pc :: starts_rev)
      | '\x0A' -> loop (pc + 1) (Add1 :: instrs_rev) (pc :: starts_rev)
      | '\x0B' -> loop (pc + 1) (Sub1 :: instrs_rev) (pc :: starts_rev)
      | '\x0C' -> loop (pc + 1) (Move1R :: instrs_rev) (pc :: starts_rev)
      | '\x0D' -> loop (pc + 1) (Move1L :: instrs_rev) (pc :: starts_rev)
      | '\x0E' -> loop (pc + 1) (Transfer1L :: instrs_rev) (pc :: starts_rev)
      | '\x0F' ->
        ensure_bounds code code_length pc 1;
        let n = read_i8 (pc + 1) in
        loop (pc + 2) (TransferN n :: instrs_rev) (pc :: starts_rev)
      | '\x10' -> loop (pc + 1) (Scan1R :: instrs_rev) (pc :: starts_rev)
      | '\x11' -> loop (pc + 1) (Scan1L :: instrs_rev) (pc :: starts_rev)
      | '\x12' ->
        ensure_bounds code code_length pc 1;
        let n = read_i8 (pc + 1) in
        loop (pc + 2) (ScanN n :: instrs_rev) (pc :: starts_rev)
      | '\x13' ->
        ensure_bounds code code_length pc 2;
        let d = read_i8 (pc + 1) in
        let n = read_i8 (pc + 2) in
        loop (pc + 3) (AddAt (d, n) :: instrs_rev) (pc :: starts_rev)
      | '\x14' ->
        ensure_bounds code code_length pc 1;
        let count = Bytes.unsafe_get code (pc + 1) |> Char.code in
        let total_pairs_bytes = count * 2 in
        ensure_bounds code code_length pc (1 + total_pairs_bytes);
        let pairs = read_pairs [] (pc + 2) count in
        loop
          (pc + 2 + total_pairs_bytes)
          (MulTransfer pairs :: instrs_rev)
          (pc :: starts_rev)
      | op -> raise (VMExn (InvalidInstruction op)))
  in
  loop 0 [] []
;;

let create
      ?(io = In_channel.stdin, Out_channel.stdout)
      ?(memory = Tape.create 256)
      program
  =
  let instrs, jump_targets = decode_program program in
  { instrs
  ; instr_count = Array.length instrs
  ; jump_targets
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

let[@inline] op_transfer ?(delta = 1) t =
  let v = Tape.get t.memory in
  if v <> 0
  then (
    Tape.set t.memory 0;
    Tape.add_at_offset_exn t.memory delta v);
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

let exec_instr t = function
  | Hang -> hang t
  | AddN n ->
    Tape.add t.memory n;
    advance t
  | MoveN n ->
    Tape.move_exn t.memory n;
    advance t
  | Jz _ ->
    if Tape.get t.memory = 0
    then (
      let target = t.jump_targets.(t.pc) in
      (* if target < 0 || target >= t.instr_count *)
      (* then raise (VMExn (JumpOutOfBounds target)) *)
      (*else*)
      advance ~replace:target t)
    else advance t
  | Jnz _ ->
    if Tape.get t.memory <> 0
    then (
      let target = t.jump_targets.(t.pc) in
      (* if target < 0 || target >= t.instr_count *)
      (* then raise (VMExn (JumpOutOfBounds target)) *)
      (*else*)
      advance ~replace:target t)
    else advance t
  | In ->
    let v = In_channel.input_char t.input |> Option.value ~default:'\000' |> Char.code in
    Tape.set t.memory v;
    advance t
  | Out ->
    Tape.get t.memory |> Char.chr |> Out_channel.output_char t.output;
    Out_channel.flush t.output;
    advance t
  | Call -> advance t
  | SetZero ->
    Tape.set t.memory 0;
    advance t
  | Transfer1R -> op_transfer t |> advance
  | Add1 ->
    Tape.add t.memory 1;
    advance t
  | Sub1 ->
    Tape.add t.memory (-1);
    advance t
  | Move1R ->
    Tape.move_exn t.memory 1;
    advance t
  | Move1L ->
    Tape.move_exn t.memory (-1);
    advance t
  | Transfer1L -> op_transfer ~delta:(-1) t |> advance
  | TransferN n -> op_transfer ~delta:n t |> advance
  | Scan1R -> op_scan t |> advance
  | Scan1L -> op_scan ~stride:(-1) t |> advance
  | ScanN n -> op_scan ~stride:n t |> advance
  | AddAt (delta, n) ->
    Tape.add_at_offset_exn t.memory delta n;
    advance t
  | MulTransfer pairs ->
    let source_value = Tape.get t.memory in
    if source_value <> 0
    then (
      Tape.set t.memory 0;
      pairs
      |> List.iter (fun (d, c) -> Tape.add_at_offset_exn t.memory d (source_value * c)));
    let size = 1 + 1 + (2 * List.length pairs) in
    ignore size;
    advance t
;;

let rec run_exn t =
  if t.pc >= t.instr_count then t else run_exn (exec_instr t t.instrs.(t.pc))
;;

let run t =
  try Ok (run_exn t) with
  | VMExn err -> Error err
  | Tape.TapeExn err -> Error (TapeError err)
;;
