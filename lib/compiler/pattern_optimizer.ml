open Core
open Isa

let rec fixed_point f x =
  let x' = f x in
  if Poly.( = ) x x' then x else fixed_point f x'
;;

let combine_pairs pairs =
  let map =
    List.fold_left
      pairs
      ~init:(Map.empty (module Int))
      ~f:(fun acc (delta, coeff) ->
        Map.update acc delta ~f:(function
          | Some existing -> existing + coeff
          | None -> coeff))
  in
  Map.to_alist map
  |> List.filter ~f:(fun (_, c) -> c <> 0)
  |> List.sort ~compare:(fun (d1, _) (d2, _) -> Int.compare d1 d2)
;;

let match_noop _ = function
  | _ -> None
;;

let match_multitransfer acc instructions =
  let rec collect pairs seen_decrement offset = function
    | [] -> None
    | CloseLoop :: rest ->
      (* valid pattern: back at origin, seen decrement, have transfers *)
      if offset = 0 && seen_decrement
      then (
        let combined = combine_pairs pairs in
        match combined with
        | [] | [ (_, 1) ] -> None (* empty or trivial single transfer *)
        | _ -> Some (combined, rest))
      else None
    | OpenLoop :: _ -> None (* nested loops not allowed *)
    | Instr (Move delta) :: rest -> collect pairs seen_decrement (offset + delta) rest
    | Instr (Add n) :: rest ->
      if offset = 0
      then
        (* at origin: check for decrement *)
        if n = -1 && not seen_decrement
        then collect pairs true offset rest
        else None (* Invalid: multiple decrements or non-unit decrement *)
      else
        (* away from origin: accumulate transfer *)
        collect ((offset, n) :: pairs) seen_decrement offset rest
    | _ :: _ -> None (* any other instruction breaks the pattern *)
  in
  match instructions with
  | OpenLoop :: rest ->
    (match collect [] false 0 rest with
     | Some (pairs, remaining) -> Some (Instr (MultiTransfer pairs) :: acc, remaining)
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
  | OpenLoop :: Instr (Add n) :: CloseLoop :: rest when n = -1 || n = 1 ->
    (* [+/-] *)
    Some (Instr Clear :: acc, rest)
  | _ -> None
;;

let match_scanstride acc = function
  | OpenLoop :: Instr (Move n) :: CloseLoop :: rest ->
    (* [k</>] *)
    Some (Instr (ScanStride n) :: acc, rest)
  | _ -> None
;;

let match_transferstride acc = function
  | OpenLoop
    :: Instr (Add n1)
    :: Instr (Move n)
    :: Instr (Add m1)
    :: Instr (Move m)
    :: CloseLoop
    :: rest
    when n1 = -1 && m1 = 1 && n <> 0 && m = -n ->
    (* [ -; k</>; +/-; k>/< ] TransferStride to left/right *)
    Some (Instr (TransferStride n) :: acc, rest)
  | OpenLoop
    :: Instr (Move n)
    :: Instr (Add m1)
    :: Instr (Move m)
    :: Instr (Add n1)
    :: CloseLoop
    :: rest
    when m1 = 1 && n1 = -1 && n <> 0 && m = -n ->
    (* [ k</>; +/-; k>/<; - ] TransferStride to left/right *)
    Some (Instr (TransferStride n) :: acc, rest)
  | _ -> None
;;

let match_addat acc = function
  | Instr (Move d1) :: Instr (Add n) :: Instr (Move d2) :: rest when d1 * d2 <= 0 ->
    (* k>/<; k+/-; k</> *)
    (* combine symmetric/partial cancellations around Add into AddAt and a leftover Move if any. *)
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
        Some (Instr (AddAt (addat_delta, n)) :: Instr (Move leftover) :: acc, rest))
      else
        (* second move overshoots: leftover after AddAt from origin of first move *)
        Some (Instr (Move leftover) :: Instr (AddAt (d1, n)) :: acc, rest))
  | _ -> None
;;

let match_trailadd acc = function
  | Instr (Move d0) :: Instr (Add n0) :: rest when d0 <> 0 && n0 <> 0 ->
    (* Move Add Move Add ... *)
    let rec collect current_offset pairs = function
      | Instr (Move dk) :: Instr (Add ck) :: rest ->
        let new_offset = current_offset + dk in
        collect new_offset ((new_offset, ck) :: pairs) rest
      | rest -> current_offset, pairs, rest
    in
    let net_movement, all_pairs, remaining = collect d0 [ d0, n0 ] rest in
    (match combine_pairs all_pairs with
     | [] | [ _ ] -> None (* require at least two pairs to be worth it *)
     | pairs ->
       if net_movement = 0
       then Some (Instr (TrailAdd pairs) :: acc, remaining)
       else Some (Instr (Move net_movement) :: Instr (TrailAdd pairs) :: acc, remaining))
  | _ -> None
;;

let match_clearcells acc = function
  | Instr Clear :: Instr (Move d) :: Instr Clear :: rest when abs d = 1 ->
    (* [-]>[-](repeat >[-])(w/o < or <<) *)
    let rec collect k = function
      | Instr (Move d2) :: Instr Clear :: rest' when d2 = d -> collect (k + 1) rest'
      | rest' -> k, rest'
    in
    let count, rest' = collect 2 rest in
    (match rest' with
     | Instr (Move back) :: tl ->
       let adj = back + (d * (count - 1)) in
       (* leftover move checks *)
       let new_tail = if adj = 0 then tl else Instr (Move adj) :: tl in
       Some (Instr (ClearCells (d * count)) :: acc, new_tail)
     | _ ->
       Some
         (Instr (Move (d * (count - 1))) :: Instr (ClearCells (d * count)) :: acc, rest'))
  | _ -> None
;;

let match_setconst acc = function
  | Instr Clear :: Instr (Add n) :: rest ->
    (* [-]; k+/- *)
    Some (Instr (SetConst n) :: acc, rest)
  | _ -> None
;;

let run instructions =
  let matchers =
    [ match_noop
    ; match_multitransfer
    ; match_call
    ; match_hang
    ; match_clear
    ; match_scanstride
    ; match_transferstride
    ; match_trailadd
    ; match_addat
    ; match_clearcells
    ; match_setconst
    ]
  in
  let rec optimize acc = function
    | [] -> List.rev acc
    | instr :: rest ->
      (match matchers |> List.find_map ~f:(fun m -> m acc (instr :: rest)) with
       | Some (acc', rest') -> optimize acc' rest'
       | None -> optimize (instr :: acc) rest)
  in
  instructions |> fixed_point (fun i -> optimize [] i)
;;
