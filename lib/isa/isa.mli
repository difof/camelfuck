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

val pp_t : Format.formatter -> t -> unit
val pp_intr : Format.formatter -> intr -> unit
val pp_error : Format.formatter -> error -> unit
val byte_size : t -> int
val to_char : t -> char
val encode : t -> (bytes, error) result
val encode_list : t list -> (bytes list, error) result
val combine_encoded_list : bytes list -> bytes
val pp_intr_indent : ?spacing:int -> Format.formatter -> intr list -> unit
