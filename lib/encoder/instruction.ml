open Stdlib

type t =
  | Add of int
  | Sub of int
  | Move of int
  | Jz of int
  | Jnz of int
  | In
  | Out
  | Call
  | SetZero
  | Copy
  | Add1
  | Sub1
  | Move1R
  | Move1L

type error = OperandOutOfBounds of (t * int * int * int)

let pp_t fmt = function
  | Add n -> Format.fprintf fmt "add %d" n
  | Sub n -> Format.fprintf fmt "sub %d" n
  | Move n -> Format.fprintf fmt "move %d" n
  | Jz n -> Format.fprintf fmt "jz %d" n
  | Jnz n -> Format.fprintf fmt "jnz %d" n
  | In -> Format.fprintf fmt "in"
  | Out -> Format.fprintf fmt "out"
  | Call -> Format.fprintf fmt "call"
  | SetZero -> Format.fprintf fmt "setzero"
  | Copy -> Format.fprintf fmt "copy"
  | Add1 -> Format.fprintf fmt "add1"
  | Sub1 -> Format.fprintf fmt "sub1"
  | Move1R -> Format.fprintf fmt "move1r"
  | Move1L -> Format.fprintf fmt "move1l"
;;

let pp_error fmt = function
  | OperandOutOfBounds (t, v, min, max) ->
    Format.fprintf fmt "Operand %d out of bounds [%d-%d] for opcode %a" v min max pp_t t
;;

let to_char t =
  let chr = Char.unsafe_chr in
  match t with
  | Add _ -> chr 0x01
  | Sub _ -> chr 0x02
  | Move _ -> chr 0x03
  | Jz _ -> chr 0x04
  | Jnz _ -> chr 0x05
  | In -> chr 0x06
  | Out -> chr 0x07
  | Call -> chr 0x08
  | SetZero -> chr 0x09
  | Copy -> chr 0x0A
  | Add1 -> chr 0x0B
  | Sub1 -> chr 0x0C
  | Move1R -> chr 0x0D
  | Move1L -> chr 0x0E
;;

let encode t =
  let op t size = to_char t |> Bytes.make size in
  let op_no_arg t = Ok (op t 1) in
  let op_with_arg t size setter v =
    let buffer = op t size in
    setter buffer 1 v;
    buffer
  in
  let op_with_byte_arg t v =
    if v < 0 || v > 255
    then Error (OperandOutOfBounds (t, v, 0, 255))
    else Ok (op_with_arg t 2 Bytes.set_uint8 v)
  in
  let op_with_int32_arg t v =
    let i = Int32.of_int v in
    if i < Int32.min_int || i > Int32.max_int
    then
      Error
        (OperandOutOfBounds
           (t, v, Int32.min_int |> Int32.to_int, Int32.max_int |> Int32.to_int))
    else Ok (op_with_arg t 5 Bytes.set_int32_le i)
  in
  match t with
  | Add n -> op_with_byte_arg t n
  | Sub n -> op_with_byte_arg t n
  | Move n -> op_with_byte_arg t n
  | Jz rel_pos -> op_with_int32_arg t rel_pos
  | Jnz rel_pos -> op_with_int32_arg t rel_pos
  | _ -> op_no_arg t
;;
