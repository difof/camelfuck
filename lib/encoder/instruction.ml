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
  | AddAt of int * int
  | MulTransfer of (int * int) list

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
  | AddAt (n, m) -> Format.fprintf fmt "addat(%d,%d)" n m
  | MulTransfer pairs ->
    let rec pp_pairs fmt = function
      | [] -> ()
      | [ (d, c) ] -> Format.fprintf fmt "(%d,%d)" d c
      | (d, c) :: tl -> Format.fprintf fmt "(%d,%d);%a" d c pp_pairs tl
    in
    Format.fprintf fmt "multransfer[%a]" pp_pairs pairs
;;

let pp_error fmt = function
  | OperandOutOfBounds (t, v, min, max) ->
    Format.fprintf fmt "Operand %d out of bounds [%d-%d] for opcode %a" v min max pp_t t
;;

let size = function
  | AddN _ | MoveN _ | TransferN _ | ScanN _ -> 2
  | AddAt _ -> 3
  | Jz _ | Jnz _ -> 5
  | MulTransfer pairs -> 2 + (2 * List.length pairs)
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
  | AddAt (_, _) -> chr 0x13
  | MulTransfer _ -> chr 0x14
;;

let encode t =
  let op t size = to_char t |> Bytes.make size in
  let op_no_arg t = Ok (op t 1) in
  let op_with_arg t size setter v =
    let buffer = op t size in
    setter buffer 1 v;
    buffer
  in
  let op_with_i8_arg t v =
    let min_v = -128 in
    let max_v = 127 in
    if v < min_v || v > max_v
    then Error (OperandOutOfBounds (t, v, min_v, max_v))
    else Ok (op_with_arg t (size t) Bytes.set_int8 v)
  in
  let op_with_i8_2_arg t (v1, v2) =
    let min_v = -128 in
    let max_v = 127 in
    if v1 < min_v || v1 > max_v
    then Error (OperandOutOfBounds (t, v1, min_v, max_v))
    else if v2 < min_v || v2 > max_v
    then Error (OperandOutOfBounds (t, v1, min_v, max_v))
    else
      Ok
        (op_with_arg
           t
           (size t)
           (fun buffer offset (v1, v2) ->
              Bytes.set_int8 buffer offset v1;
              Bytes.set_int8 buffer (offset + 1) v2)
           (v1, v2))
  in
  let op_with_int32_arg t v =
    let min_v = Int32.min_int |> Int32.to_int in
    let max_v = Int32.max_int |> Int32.to_int in
    if v < min_v || v > max_v
    then Error (OperandOutOfBounds (t, v, min_v, max_v))
    else Ok (op_with_arg t (size t) Bytes.set_int32_le @@ Int32.of_int v)
  in
  match t with
  | AddN n | MoveN n | TransferN n | ScanN n -> op_with_i8_arg t n
  | AddAt (d, n) -> op_with_i8_2_arg t (d, n)
  | Jz rel_pos | Jnz rel_pos -> op_with_int32_arg t rel_pos
  | MulTransfer pairs ->
    let count = List.length pairs in
    if count < 0 || count > 127
    then Error (OperandOutOfBounds (t, count, 0, 127))
    else (
      (* variable-length: opcode, count, then (delta, coeff)* *)
      let total = size t in
      let buffer = op t total in
      Bytes.set_uint8 buffer 1 count;
      let rec write idx = function
        | [] -> ()
        | (d, c) :: tl ->
          let min_v = -128 in
          let max_v = 127 in
          if d < min_v || d > max_v
          then raise (Invalid_argument "multransfer dest out of bounds")
          else if c < min_v || c > max_v
          then raise (Invalid_argument "multransfer coeff out of bounds")
          else (
            Bytes.set_int8 buffer idx d;
            Bytes.set_int8 buffer (idx + 1) c;
            write (idx + 2) tl)
      in
      try
        write 2 pairs;
        Ok buffer
      with
      | Invalid_argument _ ->
        (* map to OperandOutOfBounds using first offending value we can detect by re-checking *)
        let rec find_bad = function
          | [] -> None
          | (d, c) :: tl ->
            if d < -128 || d > 127
            then Some d
            else if c < -128 || c > 127
            then Some c
            else find_bad tl
        in
        (match find_bad pairs with
         | Some bad -> Error (OperandOutOfBounds (t, bad, -128, 127))
         | None -> Error (OperandOutOfBounds (t, 0, -128, 127))))
  | _ -> op_no_arg t
;;
