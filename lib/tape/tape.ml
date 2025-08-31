open Core

type t =
  { mutable pos : int
  ; mutable bias : int
  ; mutable buffer : int array
  ; mutable len : int
  ; max_size : int
  }

type bias_offset =
  | Start
  | Middle

type error =
  | MaxAllocationReached of int * int
  | BlitOutOfBounds of int * int * int
  | BlitNegativeLength of int

exception TapeExn of error

let[@inline] physical_index t = t.bias + t.pos
let[@inline] alloc n = Array.create ~len:n 0
let[@inline] mask v = v land 0xFF

let[@inline] blit_bounds_check_exn t len =
  let offset = physical_index t in
  if len < 0 then raise (TapeExn (BlitNegativeLength len));
  let last = offset + len - 1 in
  if offset < 0 || (len > 0 && (last < 0 || last >= t.len))
  then raise (TapeExn (BlitOutOfBounds (offset, len, t.len)));
  offset
;;

let realloc_exn t new_index =
  let needed = max (abs new_index + 1) t.len in
  (* grow in multples of 2 *)
  let rec grow x = if x < needed then grow (x lsl 1) else x in
  let new_len = grow (max 32 (t.len + (t.len lsr 1))) in
  if new_len > t.max_size
  then raise (TapeExn (MaxAllocationReached (new_index, t.max_size)));
  let new_buf = alloc new_len in
  let new_bias, dst =
    if new_index < 0
    then (
      (* grow to left, keep right side where it is *)
      let left_space = new_len - t.len in
      t.bias + left_space, left_space)
    else if new_index >= t.len
    then
      (* grow to right, keep left side where it is *)
      t.bias, 0
    else
      (* within bounds but reallocating *)
      new_len / 2, (new_len / 2) - t.bias
  in
  Array.blit ~src:t.buffer ~src_pos:0 ~dst:new_buf ~dst_pos:dst ~len:t.len;
  t.buffer <- new_buf;
  t.bias <- new_bias;
  t.len <- new_len
;;

let pp_error fmt = function
  | MaxAllocationReached (i, max) ->
    Format.fprintf fmt "Index %d is out of max bounds of %d" i max
  | BlitOutOfBounds (offset, size, len) ->
    Format.fprintf fmt "Blit offset %d size %d is out of tape bounds %d" offset size len
  | BlitNegativeLength len -> Format.fprintf fmt "Blit was given negative length %d" len
;;

let create ?(bias_offset = Middle) ?(max_size = 16384) initial_size =
  let len = max 256 initial_size in
  let max_size = max len max_size in
  let bias =
    match bias_offset with
    | Start -> 0
    | Middle -> len / 2
  in
  { pos = 0; bias; buffer = alloc len; len; max_size }
;;

let[@inline] move_exn t n =
  t.pos <- t.pos + n;
  let i = physical_index t in
  if i >= 0 && i < t.len then () else realloc_exn t i
;;

let[@inline] move t n =
  try
    move_exn t n;
    Ok ()
  with
  | TapeExn err -> Error err
;;

let[@inline] get t =
  let i = physical_index t in
  Array.unsafe_get t.buffer i
;;

let[@inline] set t v =
  let i = physical_index t in
  Array.unsafe_set t.buffer i (mask v)
;;

let[@inline] add t v =
  let i = physical_index t in
  let cur = Array.unsafe_get t.buffer i in
  Array.unsafe_set t.buffer i (mask (cur + v))
;;

let[@inline] set_at_offset_exn t delta v =
  let i = physical_index t + delta in
  if i < 0 || i >= t.len then realloc_exn t i;
  Array.unsafe_set t.buffer i (mask v)
;;

let[@inline] add_at_offset_exn t delta v =
  let i = physical_index t + delta in
  if i < 0 || i >= t.len then realloc_exn t i;
  let cur = Array.unsafe_get t.buffer i in
  Array.unsafe_set t.buffer i (mask (cur + v))
;;

let[@inline] add_at_offset t delta v =
  try
    add_at_offset_exn t delta v;
    Ok ()
  with
  | TapeExn err -> Error err
;;

let[@inline] len t = t.len
let[@inline] bias t = t.bias
let[@inline] logical_pos t = t.pos
let[@inline] physical_pos t = physical_index t
let[@inline] max_size t = t.max_size

let[@inline] blit_out_exn t dst len =
  let src_pos = blit_bounds_check_exn t len in
  Array.blit ~src:t.buffer ~src_pos ~dst ~dst_pos:0 ~len
;;

let[@inline] blit_in_exn t src len =
  let dst_pos = blit_bounds_check_exn t len in
  Array.blit ~src ~src_pos:0 ~dst:t.buffer ~dst_pos ~len
;;

let[@inline] blit_in_ensure_exn t src len =
  let dst_pos = physical_index t in
  let last_pos = dst_pos + len - 1 in
  if last_pos < 0 || last_pos >= t.len then realloc_exn t last_pos;
  Array.blit ~src ~src_pos:0 ~dst:t.buffer ~dst_pos ~len
;;
