type t =
  | MaxAllocationReached of int * int
  | BlitOutOfBounds of int * int * int
  | BlitNegativeLength of int

exception Exception of t

let pp_error fmt = function
  | MaxAllocationReached (i, max) ->
    Format.fprintf fmt "Index %d is out of max bounds of %d" i max
  | BlitOutOfBounds (offset, size, len) ->
    Format.fprintf fmt "Blit offset %d size %d is out of tape bounds %d" offset size len
  | BlitNegativeLength len -> Format.fprintf fmt "Blit was given negative length %d" len
;;
