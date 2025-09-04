open Core

type multi_op = (int * int) list

type t =
  | Hang
  | Add of int
  | AddAt of int * int
  | Move of int
  | Jz of int
  | Jnz of int
  | ScanStride of int
  | TransferStride of int
  | MultiTransfer of multi_op
  | In
  | Out
  | Call
  | Clear
  | ClearCells of int
  | SetConst of int
  | TrailAdd of multi_op

type intr =
  | Instr of t
  | OpenLoop
  | CloseLoop

type error = OperandOutOfBounds of (t * int * int * int)

let rec pp_pairs fmt = function
  | [] -> ()
  | [ (d, c) ] -> Format.fprintf fmt "(%d,%d)" d c
  | (d, c) :: tl -> Format.fprintf fmt "(%d,%d);%a" d c pp_pairs tl
;;

let pp_t fmt = function
  | Hang -> Format.fprintf fmt "Hang"
  | Add n -> Format.fprintf fmt "Add(%d)" n
  | Move n -> Format.fprintf fmt "Move(%d)" n
  | Jz n -> Format.fprintf fmt "Jz(%d)" n
  | Jnz n -> Format.fprintf fmt "Jnz(%d)" n
  | In -> Format.fprintf fmt "In"
  | Out -> Format.fprintf fmt "Out"
  | Call -> Format.fprintf fmt "Call"
  | Clear -> Format.fprintf fmt "Clear"
  | TransferStride n -> Format.fprintf fmt "TransferStride(%d)" n
  | ScanStride n -> Format.fprintf fmt "ScanStride(%d)" n
  | AddAt (n, m) -> Format.fprintf fmt "AddAt(%d,%d)" n m
  | MultiTransfer pairs -> Format.fprintf fmt "MultiTransfer[%a]" pp_pairs pairs
  | ClearCells n -> Format.fprintf fmt "ClearCells(%d)" n
  | SetConst n -> Format.fprintf fmt "SetConst(%d)" n
  | TrailAdd pairs -> Format.fprintf fmt "TrailAdd[%a]" pp_pairs pairs
;;

let pp_intr fmt = function
  | Instr t -> pp_t fmt t
  | OpenLoop -> Format.fprintf fmt "OpenLoop"
  | CloseLoop -> Format.fprintf fmt "CloseLoop"
;;

let pp_error fmt = function
  | OperandOutOfBounds (t, v, min, max) ->
    Format.fprintf fmt "Operand %d out of bounds [%d-%d] for opcode %a" v min max pp_t t
;;

let byte_size = function
  | Add _ | Move _ | TransferStride _ | ScanStride _ | ClearCells _ | SetConst _ -> 2
  | AddAt _ -> 3
  | Jz _ | Jnz _ -> 5
  | MultiTransfer pairs | TrailAdd pairs -> 2 + (2 * List.length pairs)
  | _ -> 1
;;

let to_char t =
  let chr = Stdlib.Char.unsafe_chr in
  match t with
  | Hang -> chr 0x00
  | Add _ -> chr 0x01
  | Move _ -> chr 0x02
  | Jz _ -> chr 0x03
  | Jnz _ -> chr 0x04
  | In -> chr 0x05
  | Out -> chr 0x06
  | Call -> chr 0x07
  | Clear -> chr 0x08
  | TransferStride _ -> chr 0x09
  | ScanStride _ -> chr 0x0A
  | AddAt (_, _) -> chr 0x0B
  | MultiTransfer _ -> chr 0x0C
  | ClearCells _ -> chr 0x0E
  | SetConst _ -> chr 0x0F
  | TrailAdd _ -> chr 0x10
;;

let encode t =
  (* TODO: use Core.Bytes *)
  (* TODO: improve MultiTransfer *)
  let open Stdlib in
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
    else Ok (op_with_arg t (byte_size t) Bytes.set_int8 v)
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
           (byte_size t)
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
    else Ok (op_with_arg t (byte_size t) Bytes.set_int32_le @@ Int32.of_int v)
  in
  match t with
  | Add n | Move n | TransferStride n | ScanStride n | SetConst n -> op_with_i8_arg t n
  | ClearCells n -> op_with_i8_arg t n
  | AddAt (d, n) -> op_with_i8_2_arg t (d, n)
  | Jz rel_pos | Jnz rel_pos -> op_with_int32_arg t rel_pos
  | MultiTransfer pairs | TrailAdd pairs ->
    let count = List.length pairs in
    if count < 0 || count > 127
    then Error (OperandOutOfBounds (t, count, 0, 127))
    else (
      (* variable-length: opcode, count, then (delta, coeff)* *)
      let total = byte_size t in
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

let encode_list instructions =
  let open Result in
  let folder acc instr =
    acc
    >>= fun acc ->
    match encode instr with
    | Ok bytes -> Ok (bytes :: acc)
    | Error _ as err -> err
  in
  instructions |> List.fold ~init:(Ok []) ~f:folder >>| List.rev
;;

let combine_encoded_list instr_bytes_list =
  let code_size =
    instr_bytes_list |> List.fold ~init:0 ~f:(fun acc instr -> Bytes.length instr + acc)
  in
  let code = Bytes.make code_size '\000' in
  instr_bytes_list
  |> List.fold ~init:0 ~f:(fun pos instr ->
    let len = Bytes.length instr in
    Bytes.blit ~src:instr ~src_pos:0 ~dst:code ~dst_pos:pos ~len;
    pos + len)
  |> ignore;
  code
;;

let pp_intr_indent ?(spacing = 1) fmt intr_ls =
  let unit =
    let sp = Int.max 0 spacing in
    "|" ^ String.make sp ' '
  in
  let bar_prefix indent =
    if indent <= 0
    then ""
    else String.concat ~sep:"" (List.init indent ~f:(fun _ -> unit))
  in
  intr_ls
  |> List.fold ~init:0 ~f:(fun indent intr ->
    match intr with
    | Instr t ->
      Format.fprintf fmt "%s%a\n" (bar_prefix indent) pp_t t;
      indent
    | OpenLoop ->
      Format.fprintf fmt "%sOpenLoop\n" (bar_prefix indent);
      indent + 1
    | CloseLoop ->
      let new_indent = Int.max 0 (indent - 1) in
      Format.fprintf fmt "%sCloseLoop\n" (bar_prefix new_indent);
      new_indent)
  |> ignore
;;
