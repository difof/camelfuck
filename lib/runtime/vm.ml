open Core
open Isa

type t =
  { program : Isa.t array
  ; program_length : int
  ; memory : Tape.t
  ; input : In_channel.t
  ; output : Out_channel.t
  ; mutable pc : int
  ; obuf : Bytes.t
  ; obuf_len : int
  ; mutable obuf_pos : int
  }

type error =
  | InvalidInstruction of int
  | BytecodeOutOfBounds of int
  | JumpOutOfBounds of int
  | TapeError of Tape.error
  | Exception of exn

exception VMExn of error

let pp_error fmt = function
  | InvalidInstruction op -> Format.fprintf fmt "Invalid instruction: %x" op
  | BytecodeOutOfBounds pos ->
    Format.fprintf fmt "Bytecode out of bounds at position %d" pos
  | JumpOutOfBounds pos -> Format.fprintf fmt "Jump out of bounds to position %d" pos
  | TapeError err -> Tape.pp_error fmt err
  | Exception ex -> Format.fprintf fmt "FUCK %s" (Stdlib.Printexc.to_string ex)
;;

let create
      ?(io = In_channel.stdin, Out_channel.stdout)
      ?(memory = Tape.create 32768)
      program
  =
  let obuf_len = 16 in
  { program = Array.of_list program
  ; program_length = List.length program
  ; memory
  ; input = fst io
  ; output = snd io
  ; pc = 0
  ; obuf = Bytes.make obuf_len '\000'
  ; obuf_len
  ; obuf_pos = 0
  }
;;

let[@inline always] flush_emit t =
  Out_channel.output_bytes t.output t.obuf;
  Out_channel.flush t.output;
  Bytes.fill t.obuf ~pos:0 ~len:t.obuf_len '\000';
  t.obuf_pos <- 0
;;

let[@inline always] emit_byte t (c : char) =
  if t.obuf_pos >= t.obuf_len then flush_emit t;
  Bytes.unsafe_set t.obuf t.obuf_pos c;
  t.obuf_pos <- t.obuf_pos + 1
;;

let hang () =
  while true do
    ()
  done
;;

let[@inline] op_transfer t delta = Tape.transfer_exn t.memory delta
let[@inline] op_scan t delta = Tape.scan_to_zero_exn t.memory delta
let[@inline] op_multransfer t pairs = Tape.multransfer_exn t.memory pairs

let[@inline] op_clearn t move n =
  Tape.mulclear_exn t.memory n;
  if move
  then (
    let disp = if n > 0 then n - 1 else if n < 0 then n + 1 else 0 in
    Tape.move_exn t.memory disp)
;;

let run_exn t =
  let len = t.program_length in
  while t.pc < len do
    match t.program.(t.pc) with
    | Hang -> hang ()
    | AddN n ->
      Tape.add t.memory n;
      t.pc <- t.pc + 1
    | MoveN n ->
      Tape.move_exn t.memory n;
      t.pc <- t.pc + 1
    | Jz rel -> if Tape.get t.memory = 0 then t.pc <- t.pc + rel else t.pc <- t.pc + 1
    | Jnz rel -> if Tape.get t.memory <> 0 then t.pc <- t.pc + rel else t.pc <- t.pc + 1
    | In ->
      flush_emit t;
      let v =
        In_channel.input_char t.input
        |> Option.value ~default:'\000'
        |> Stdlib.int_of_char
      in
      Tape.set t.memory v;
      t.pc <- t.pc + 1
    | Out ->
      emit_byte t (Tape.get t.memory |> Stdlib.char_of_int);
      t.pc <- t.pc + 1
    | Call ->
      (* placeholder for runtime call *)
      t.pc <- t.pc + 1
    | Clear ->
      Tape.set t.memory 0;
      t.pc <- t.pc + 1
    | TransferN n ->
      op_transfer t n;
      t.pc <- t.pc + 1
    | ScanN n ->
      op_scan t n;
      t.pc <- t.pc + 1
    | AddAt (delta, n) ->
      Tape.add_at_offset_exn t.memory delta n;
      t.pc <- t.pc + 1
    | MulTransfer pairs ->
      op_multransfer t pairs;
      t.pc <- t.pc + 1
    | ClearN (n, move) ->
      op_clearn t move n;
      t.pc <- t.pc + 1
    | SetConst n ->
      Tape.set t.memory n;
      t.pc <- t.pc + 1
  done
;;

let run t =
  let r =
    try Ok (run_exn t) with
    | VMExn err -> Error err
    | Tape.TapeExn err -> Error (TapeError err)
    | ex -> Error (Exception ex)
  in
  flush_emit t;
  r
;;
