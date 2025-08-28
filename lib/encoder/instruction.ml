open Stdlib

type t =
  | Hang
  | AddN of int
  | MoveN of int
  | Jz of int
  | Jnz of int
  | In
  | Out
  | Call
  | SetZero
  | Transfer1R
  | Transfer1L
  | TransferN of int
  | Add1
  | Sub1
  | Move1R
  | Move1L
  | Scan1R
  | Scan1L
  | ScanN of int

type error = OperandOutOfBounds of (t * int * int * int)

let pp_t fmt = function
  | Hang -> Format.fprintf fmt "hang"
  | AddN n -> Format.fprintf fmt "addn(%d)" n
  | MoveN n -> Format.fprintf fmt "moven(%d)" n
  | Jz n -> Format.fprintf fmt "jz(%d)" n
  | Jnz n -> Format.fprintf fmt "jnz(%d)" n
  | In -> Format.fprintf fmt "in"
  | Out -> Format.fprintf fmt "out"
  | Call -> Format.fprintf fmt "call"
  | SetZero -> Format.fprintf fmt "setzero"
  | Transfer1R -> Format.fprintf fmt "transfer1r"
  | Transfer1L -> Format.fprintf fmt "transfer1l"
  | TransferN n -> Format.fprintf fmt "transfern(%d)" n
  | Add1 -> Format.fprintf fmt "add1"
  | Sub1 -> Format.fprintf fmt "sub1"
  | Move1R -> Format.fprintf fmt "move1r"
  | Move1L -> Format.fprintf fmt "move1l"
  | Scan1R -> Format.fprintf fmt "scan1r"
  | Scan1L -> Format.fprintf fmt "scan1l"
  | ScanN n -> Format.fprintf fmt "scann(%d)" n
;;

let pp_error fmt = function
  | OperandOutOfBounds (t, v, min, max) ->
    Format.fprintf fmt "Operand %d out of bounds [%d-%d] for opcode %a" v min max pp_t t
;;

let size = function
  | AddN _ | MoveN _ | TransferN _ | ScanN _ -> 2
  | Jz _ | Jnz _ -> 5
  | _ -> 1
;;

let to_char t =
  let chr = Char.unsafe_chr in
  match t with
  | Hang -> chr 0x00
  | AddN _ -> chr 0x01
  | MoveN _ -> chr 0x02
  | Jz _ -> chr 0x03
  | Jnz _ -> chr 0x04
  | In -> chr 0x05
  | Out -> chr 0x06
  | Call -> chr 0x07
  | SetZero -> chr 0x08
  | Transfer1R -> chr 0x09
  | Transfer1L -> chr 0x0E
  | TransferN _ -> chr 0x0F
  | Add1 -> chr 0x0A
  | Sub1 -> chr 0x0B
  | Move1R -> chr 0x0C
  | Move1L -> chr 0x0D
  | Scan1R -> chr 0x10
  | Scan1L -> chr 0x11
  | ScanN _ -> chr 0x12
;;

let encode t =
  let op t size = to_char t |> Bytes.make size in
  let op_no_arg t = Ok (op t 1) in
  let op_with_arg t size setter v =
    let buffer = op t size in
    setter buffer 1 v;
    buffer
  in
  let op_with_int8_arg t v =
    let min_v = -128 in
    let max_v = 127 in
    if v < min_v || v > max_v
    then Error (OperandOutOfBounds (t, v, min_v, max_v))
    else Ok (op_with_arg t (size t) Bytes.set_int8 v)
  in
  let op_with_int32_arg t v =
    let min_v = Int32.min_int |> Int32.to_int in
    let max_v = Int32.max_int |> Int32.to_int in
    if v < min_v || v > max_v
    then Error (OperandOutOfBounds (t, v, min_v, max_v))
    else Ok (op_with_arg t (size t) Bytes.set_int32_le @@ Int32.of_int v)
  in
  match t with
  | AddN n | MoveN n | TransferN n | ScanN n -> op_with_int8_arg t n
  | Jz rel_pos | Jnz rel_pos -> op_with_int32_arg t rel_pos
  | _ -> op_no_arg t
;;
