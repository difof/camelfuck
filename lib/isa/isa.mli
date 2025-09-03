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
  | SetConst of int

type intr =
  | Instr of t
  | OpenLoop
  | CloseLoop

val pp_t : Format.formatter -> t -> unit
val pp_intr : Format.formatter -> intr -> unit
val pp_intr_indent : ?spacing:int -> Format.formatter -> intr list -> unit
