open Core
open Isa

type error =
  | InvalidInstruction of int
  | BytecodeOutOfBounds of int
  | JumpOutOfBounds of int
  | TapeError of Tape.TapeError.t
  | Exception of exn

exception VMExn of error

let pp_error fmt = function
  | InvalidInstruction op -> Format.fprintf fmt "Invalid instruction: %x" op
  | BytecodeOutOfBounds pos ->
    Format.fprintf fmt "Bytecode out of bounds at position %d" pos
  | JumpOutOfBounds pos -> Format.fprintf fmt "Jump out of bounds to position %d" pos
  | TapeError err -> Tape.TapeError.pp_error fmt err
  | Exception ex -> Format.fprintf fmt "FUCK %s" (Stdlib.Printexc.to_string ex)
;;

module Make (Tape_intf : Tape.S) = struct
  type t =
    { program : Isa.t array
    ; program_length : int
    ; memory : Tape_intf.t
    ; input : In_channel.t
    ; output : Out_channel.t
    ; mutable pc : int
    ; obuf : Bytes.t
    ; obuf_len : int
    ; mutable obuf_pos : int
    }

  let create ?(io = In_channel.stdin, Out_channel.stdout) memory program =
    let obuf_len = 32 in
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

  let run_exn t =
    let len = t.program_length in
    while t.pc < len do
      match t.program.(t.pc) with
      | Hang -> hang ()
      | Add n ->
        Tape_intf.add t.memory n;
        t.pc <- t.pc + 1
      | Move n ->
        Tape_intf.move t.memory n;
        t.pc <- t.pc + 1
      | Jz rel ->
        if Tape_intf.get t.memory = 0 then t.pc <- t.pc + rel else t.pc <- t.pc + 1
      | Jnz rel ->
        if Tape_intf.get t.memory <> 0 then t.pc <- t.pc + rel else t.pc <- t.pc + 1
      | In ->
        flush_emit t;
        let v =
          In_channel.input_char t.input
          |> Option.value ~default:'\000'
          |> Stdlib.int_of_char
        in
        Tape_intf.set t.memory v;
        t.pc <- t.pc + 1
      | Out ->
        emit_byte t (Tape_intf.get t.memory |> Stdlib.char_of_int);
        t.pc <- t.pc + 1
      | Call ->
        (* placeholder for runtime call *)
        t.pc <- t.pc + 1
      | Clear ->
        Tape_intf.set t.memory 0;
        t.pc <- t.pc + 1
      | TransferStride stride ->
        Tape_intf.transfer t.memory stride;
        t.pc <- t.pc + 1
      | ScanStride stride ->
        Tape_intf.scan_to_zero t.memory stride;
        t.pc <- t.pc + 1
      | AddAt (delta, n) ->
        Tape_intf.add_at_offset t.memory delta n;
        t.pc <- t.pc + 1
      | MultiTransfer pairs ->
        Tape_intf.multransfer t.memory pairs;
        t.pc <- t.pc + 1
      | ClearCells n ->
        Tape_intf.mulclear t.memory n;
        t.pc <- t.pc + 1
      | SetConst n ->
        Tape_intf.set t.memory n;
        t.pc <- t.pc + 1
      | TrailAdd pairs ->
        Tape_intf.trailadd t.memory pairs;
        t.pc <- t.pc + 1
    done
  ;;

  let run t =
    let r =
      try Ok (run_exn t) with
      | VMExn err -> Error err
      | Tape.TapeError.Exception err -> Error (TapeError err)
      | ex -> Error (Exception ex)
    in
    flush_emit t;
    r
  ;;
end
