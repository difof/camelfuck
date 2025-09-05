open Core

type t =
  { mutable pos : int
  ; bias : int
  ; buffer : int array
  ; len : int
  }

let[@inline always] physical_index t = t.bias + t.pos
let[@inline always] alloc n = Array.create ~len:n 0
let[@inline always] mask v = v land 0xFF

let create size =
  let len = max 8 size in
  let bias = len / 2 in
  { pos = 0; bias; buffer = alloc len; len }
;;

let[@inline] move t n = t.pos <- t.pos + n

let scan_to_zero t delta =
  let stride = abs delta in
  let dir = if delta > 0 then 1 else -1 in
  let rec scan i =
    let next = i + (dir * stride) in
    if Array.unsafe_get t.buffer next = 0 then t.pos <- next - t.bias else scan next
  in
  let i = physical_index t in
  if Array.unsafe_get t.buffer i <> 0 then scan i
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

let[@inline] set_at_offset t delta v =
  let i = physical_index t + delta in
  Array.unsafe_set t.buffer i (mask v)
;;

let[@inline] add_at_offset t delta v =
  let i = physical_index t + delta in
  let cur = Array.unsafe_get t.buffer i in
  Array.unsafe_set t.buffer i (mask (cur + v))
;;

let[@inline] mulclear t delta =
  let count = abs delta in
  if count <> 0
  then (
    let base = physical_index t in
    let dir = if delta > 0 then 1 else -1 in
    let rec loop step =
      if step < count
      then (
        let idx = base + (dir * step) in
        Array.unsafe_set t.buffer idx 0;
        loop (step + 1))
    in
    loop 0)
;;

let[@inline] trailadd t pairs = pairs |> List.iter ~f:(fun (d, c) -> add_at_offset t d c)

let[@inline] multransfer t pairs =
  let source_value = get t in
  if source_value <> 0
  then (
    set t 0;
    List.iter pairs ~f:(fun (d, c) -> add_at_offset t d (source_value * c)))
;;

let[@inline] transfer t delta =
  let i = physical_index t in
  let source_value = Array.unsafe_get t.buffer i in
  if source_value <> 0
  then (
    Array.unsafe_set t.buffer i 0;
    let j = i + delta in
    let dest = Array.unsafe_get t.buffer j in
    Array.unsafe_set t.buffer j (mask (dest + source_value)))
;;

let[@inline] len t = t.len
let[@inline] bias t = t.bias
let[@inline] logical_pos t = t.pos
let[@inline] physical_pos t = physical_index t

let[@inline] blit_out t dst len =
  let src_pos = physical_index t in
  Array.blit ~src:t.buffer ~src_pos ~dst ~dst_pos:0 ~len
;;

let[@inline] blit_in t src len =
  let dst_pos = physical_index t in
  Array.blit ~src ~src_pos:0 ~dst:t.buffer ~dst_pos ~len
;;
