type t =
  | Hang
  | AddN of int
  | AddAt of int * int
  | MoveN of int
  | Jz of int
  | Jnz of int
  | ScanN of int
  | TransferN of int
  | MulTransfer of (int * int) list
  | In
  | Out
  | Call
  | Clear
  | ClearN of int * bool

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
