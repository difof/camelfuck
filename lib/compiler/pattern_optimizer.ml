open Core
open Isa

let rec fixed_point f x =
  let x' = f x in
  if Poly.( = ) x x' then x else fixed_point f x'
;;

let match_noop _ = function
  | _ -> None
;;

let match_multransfer acc instructions =
  (* add or update coefficient for a given delta *)
  let rec update_pairs acc pairs delta coeff =
    match pairs with
    | [] -> List.rev ((delta, coeff) :: acc)
    | (d, c) :: rest when d = delta -> List.rev_append acc ((d, c + coeff) :: rest)
    | hd :: rest -> update_pairs (hd :: acc) rest delta coeff
  in
  let rec collect pairs seen_decrement offset = function
    | [] -> None
    | CloseLoop :: rest ->
      (* valid pattern: back at origin, seen decrement, have transfers *)
      if offset = 0 && seen_decrement
      then (
        let non_zero = List.filter pairs ~f:(fun (_, c) -> c <> 0) in
        match non_zero with
        | [] | [ (_, 1) ] -> None (* empty or trivial single transfer *)
        | _ ->
          let sorted =
            List.sort non_zero ~compare:(fun (d1, _) (d2, _) -> Int.compare d1 d2)
          in
          Some (sorted, rest))
      else None
    | OpenLoop :: _ -> None (* nested loops not allowed *)
    | Instr (MoveN delta) :: rest -> collect pairs seen_decrement (offset + delta) rest
    | Instr (AddN n) :: rest ->
      if offset = 0
      then
        (* at origin: check for decrement *)
        if n = -1 && not seen_decrement
        then collect pairs true offset rest
        else None (* Invalid: multiple decrements or non-unit decrement *)
      else
        (* away from origin: accumulate transfer *)
        collect (update_pairs [] pairs offset n) seen_decrement offset rest
    | _ :: _ -> None (* any other instruction breaks the pattern *)
  in
  match instructions with
  | OpenLoop :: rest ->
    (match collect [] false 0 rest with
     | Some (pairs, remaining) -> Some (Instr (MulTransfer pairs) :: acc, remaining)
     | None -> None)
  | _ -> None
;;

let match_call acc = function
  | OpenLoop :: OpenLoop :: OpenLoop :: CloseLoop :: CloseLoop :: CloseLoop :: rest ->
    (* [[[]]] runtime extension call *)
    Some (Instr Call :: acc, rest)
  | _ -> None
;;

let match_hang acc = function
  (* []/[[]] intentional halt *)
  | OpenLoop :: CloseLoop :: rest -> Some (Instr Hang :: acc, rest)
  | OpenLoop :: OpenLoop :: CloseLoop :: CloseLoop :: rest ->
    Some (Instr Hang :: acc, rest)
  | _ -> None
;;

let match_clear acc = function
  | OpenLoop :: Instr (AddN n) :: CloseLoop :: rest when n = -1 || n = 1 ->
    (* [+/-] *)
    Some (Instr Clear :: acc, rest)
  | _ -> None
;;

let match_scann acc = function
  | OpenLoop :: Instr (MoveN n) :: CloseLoop :: rest ->
    (* [k</>] *)
    Some (Instr (ScanN n) :: acc, rest)
  | _ -> None
;;

let match_transfern acc = function
  | OpenLoop
    :: Instr (AddN n1)
    :: Instr (MoveN n)
    :: Instr (AddN m1)
    :: Instr (MoveN m)
    :: CloseLoop
    :: rest
    when n1 = -1 && m1 = 1 && n <> 0 && m = -n ->
    (* [ -; k</>; +/-; k>/< ] TransferN to left/right *)
    Some (Instr (TransferN n) :: acc, rest)
  | OpenLoop
    :: Instr (MoveN n)
    :: Instr (AddN m1)
    :: Instr (MoveN m)
    :: Instr (AddN n1)
    :: CloseLoop
    :: rest
    when m1 = 1 && n1 = -1 && n <> 0 && m = -n ->
    (* [ k</>; +/-; k>/<; - ] TransferN to left/right *)
    Some (Instr (TransferN n) :: acc, rest)
  | _ -> None
;;

let match_addat acc = function
  | Instr (MoveN d1) :: Instr (AddN n) :: Instr (MoveN d2) :: rest when d1 * d2 <= 0 ->
    (* k>/<; k+/-; k</> *)
    (* combine symmetric/partial cancellations around AddN into AddAt and a leftover MoveN if any. *)
    if d1 = -d2
    then
      (* exact cancel *)
      Some (Instr (AddAt (d1, n)) :: acc, rest)
    else (
      let leftover = d1 + d2 in
      if abs d1 > abs d2
      then (
        (* first move overshoots: leftover before returning to origin of AddAt location *)
        let addat_delta = -d2 in
        Some (Instr (AddAt (addat_delta, n)) :: Instr (MoveN leftover) :: acc, rest))
      else
        (* second move overshoots: leftover after AddAt from origin of first move *)
        Some (Instr (MoveN leftover) :: Instr (AddAt (d1, n)) :: acc, rest))
  | _ -> None
;;

let match_clearn acc = function
  | Instr Clear :: Instr (MoveN d) :: Instr Clear :: rest when abs d = 1 ->
    (* [-]>[-](repeat >[-])(w/o < or <<) *)
    let rec collect k = function
      | Instr (MoveN d2) :: Instr Clear :: rest' when d2 = d -> collect (k + 1) rest'
      | rest' -> k, rest'
    in
    let count, rest' = collect 2 rest in
    (match rest' with
     | Instr (MoveN back) :: tl ->
       let adj = back + (d * (count - 1)) in
       (* leftover move checks *)
       let new_tail = if adj = 0 then tl else Instr (MoveN adj) :: tl in
       Some (Instr (ClearN (d * count, false)) :: acc, new_tail)
     | _ -> Some (Instr (ClearN (d * count, true)) :: acc, rest'))
  | _ -> None
;;

let match_setconst acc = function
  | Instr Clear :: Instr (AddN n) :: rest ->
    (* [-]; k+/- *)
    Some (Instr (SetConst n) :: acc, rest)
  | _ -> None
;;

let run instructions =
  let matchers =
    [ match_noop
    ; match_multransfer
    ; match_call
    ; match_hang
    ; match_clear
    ; match_scann
    ; match_transfern
    ; match_addat
    ; match_muladd
    ; match_clearn
    ; match_setconst
    ]
  in
  let rec optimize acc = function
    | [] -> List.rev acc
    | hd :: tl ->
      (match matchers |> List.find_map ~f:(fun m -> m acc (hd :: tl)) with
       | Some (acc', rest) -> optimize acc' rest
       | None -> optimize (hd :: acc) tl)
  in
  instructions |> fixed_point (fun i -> optimize [] i)
;;
