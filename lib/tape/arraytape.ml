open Core

let page_size = 256

type t =
  { mutable pos : int
  ; mutable right : int array
  ; mutable left : int array
  }

let create () =
  { pos = 0; right = Array.create ~len:page_size 0; left = Array.create ~len:page_size 0 }
;;

let[@inline] pos t = t.pos
let[@inline] get t = if t.pos >= 0 then t.right.(t.pos) else t.left.(-t.pos)

let[@inline] set t v =
  let v = v land 0xFF in
  if t.pos >= 0 then t.right.(t.pos) <- v else t.left.(-t.pos) <- v
;;

let[@inline] realloc arr arr_len target_pos =
  let sz = max (target_pos + 1) (arr_len * 2) in
  let new_tape = Array.create ~len:sz 0 in
  Array.blit ~src:arr ~src_pos:0 ~dst:new_tape ~dst_pos:0 ~len:arr_len;
  new_tape
;;

let[@inline] move t n =
  t.pos
  <- (match t.pos + n with
      | n when n >= 0 ->
        let len = Array.length t.right in
        if n >= len then t.right <- realloc t.right len n;
        n
      | n ->
        let len = Array.length t.left in
        if -n >= len then t.left <- realloc t.left len (-n);
        n)
;;
