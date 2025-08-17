open Core

type t =
  { mutable pos : int
  ; cells : int Int.Table.t
  }

let create () = { pos = 0; cells = Hashtbl.create (module Int) }
let[@inline] get t = Hashtbl.find t.cells t.pos |> Option.value ~default:0

let[@inline] set t v =
  let v = v land 0xFF in
  Hashtbl.set t.cells ~key:t.pos ~data:v
;;

let[@inline] pos t = t.pos
let[@inline] move t n = t.pos <- t.pos + n
