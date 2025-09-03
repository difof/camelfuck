open Core
open Isa

type error =
  | UnmatchedClosingBracket of int
  | UnmatchedOpeningBracket of int

let pp_error fmt = function
  | UnmatchedClosingBracket i ->
    Format.fprintf fmt "Unmatched closing bracket ']' at instruction %d" i
  | UnmatchedOpeningBracket i ->
    Format.fprintf fmt "Unmatched opening bracket '[' at instruction %d" i
;;

let rec fixed_point f x =
  let x' = f x in
  if Poly.( = ) x x' then x else fixed_point f x'
;;

let parse_sequence source =
  (* count consecutive identical characters *)
  let rec count_consecutive pos target_char acc =
    if pos >= String.length source || not (Char.equal source.[pos] target_char)
    then acc, pos
    else count_consecutive (pos + 1) target_char (acc + 1)
  in
  let rec chunkify cap remaining acc =
    if remaining <= 0
    then List.rev acc
    else (
      let take = Int.min cap remaining in
      chunkify cap (remaining - take) (take :: acc))
  in
  let accumulate_chunks acc pos target_char instr =
    let count, next_pos = count_consecutive pos target_char 0 in
    let cap =
      match target_char with
      | '-' | '<' -> 128
      | _ -> 127
    in
    let chunks = chunkify cap count [] |> List.map ~f:(fun n -> instr n) in
    List.fold chunks ~init:acc ~f:(fun acc i -> i :: acc), next_pos
  in
  let rec parse acc pos =
    if pos >= String.length source
    then List.rev acc
    else (
      match source.[pos] with
      | '+' ->
        let acc, next_pos = accumulate_chunks acc pos '+' (fun n -> Instr (AddN n)) in
        parse acc next_pos
      | '-' ->
        let acc, next_pos = accumulate_chunks acc pos '-' (fun n -> Instr (AddN (-n))) in
        parse acc next_pos
      | '>' ->
        let acc, next_pos = accumulate_chunks acc pos '>' (fun n -> Instr (MoveN n)) in
        parse acc next_pos
      | '<' ->
        let acc, next_pos = accumulate_chunks acc pos '<' (fun n -> Instr (MoveN (-n))) in
        parse acc next_pos
      | '.' -> parse (Instr Out :: acc) (pos + 1)
      | ',' -> parse (Instr In :: acc) (pos + 1)
      | '[' -> parse (OpenLoop :: acc) (pos + 1)
      | ']' -> parse (CloseLoop :: acc) (pos + 1)
      | _ -> parse acc (pos + 1))
  in
  parse [] 0
;;

let fuse_std_ops instructions =
  let rec chunkify acc sum ctor =
    let v_min = -128 in
    let v_max = 127 in
    match sum with
    | 0 -> acc
    | v when v > v_max -> chunkify (ctor v_max :: acc) (v - v_max) ctor
    | v when v < v_min -> chunkify (ctor v_min :: acc) (v - v_min) ctor
    | v -> ctor v :: acc
  in
  let rec optimize acc = function
    | [] -> List.rev acc
    | Instr (AddN 0) :: rest | Instr (MoveN 0) :: rest ->
      (* this should never happen but we use it anyways *)
      optimize acc rest
    | Instr (AddN n) :: Instr (AddN m) :: rest ->
      let acc = chunkify acc (n + m) (fun v -> Instr (AddN v)) in
      optimize acc rest
    | Instr (MoveN n) :: Instr (MoveN m) :: rest ->
      let acc = chunkify acc (n + m) (fun v -> Instr (MoveN v)) in
      optimize acc rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  fixed_point (fun instrs -> optimize [] instrs) instructions
;;

(* Try matching a multiplication transfer loop: zero source, distribute to offsets, return to origin *)
let try_multransfer =
  let add_or_update pairs delta coeff =
    let rec loop acc = function
      | [] -> List.rev ((delta, coeff) :: acc)
      | (d, c) :: tl when d = delta -> List.rev_append acc ((d, c + coeff) :: tl)
      | hd :: tl -> loop (hd :: acc) tl
    in
    loop [] pairs
  in
  let rec collect pairs seen_dec cur_off = function
    | [] -> None
    | OpenLoop :: _ -> None
    | CloseLoop :: rest ->
      if cur_off = 0 && seen_dec
      then (
        let nz = List.filter pairs ~f:(fun (_, c) -> c <> 0) in
        match nz with
        | [] -> None
        | [ (_, 1) ] -> None
        | _ ->
          let merged =
            (* already merged by add_or_update; sort is enough *)
            nz |> List.sort ~compare:(fun (d1, _) (d2, _) -> compare d1 d2)
          in
          Some (merged, rest))
      else None
    | Instr (MoveN d) :: tl -> collect pairs seen_dec (cur_off + d) tl
    | Instr (AddN k) :: tl ->
      if cur_off = 0
      then if k = -1 && not seen_dec then collect pairs true cur_off tl else None
      else collect (add_or_update pairs cur_off k) seen_dec cur_off tl
    | _ :: _ -> None
  in
  function
  | Instr (AddN _) :: _ as tl -> collect [] false 0 tl
  | Instr (MoveN _) :: _ as tl -> collect [] false 0 tl
  | CloseLoop :: _ -> None
  | _ -> None
;;

let optimize_patterns instructions =
  let rec optimize acc = function
    | [] -> List.rev acc
    | OpenLoop :: tl ->
      (match try_multransfer tl with
       | Some (pairs, rest) -> optimize (Instr (MulTransfer pairs) :: acc) rest
       | None ->
         (match tl with
          | OpenLoop :: OpenLoop :: CloseLoop :: CloseLoop :: CloseLoop :: rest ->
            (* [[[]]] runtime extension call *)
            optimize (Instr Call :: acc) rest
          | CloseLoop :: rest | OpenLoop :: CloseLoop :: CloseLoop :: rest ->
            (* []/[[]] intentional halt *)
            optimize (Instr Hang :: acc) rest
          | Instr (AddN n) :: CloseLoop :: rest when n = -1 || n = 1 ->
            (* [+/-] *)
            optimize (Instr Clear :: acc) rest
          | Instr (MoveN n) :: CloseLoop :: rest ->
            (* [k</k>] *)
            optimize (Instr (ScanN n) :: acc) rest
          | Instr (AddN n1)
            :: Instr (MoveN n)
            :: Instr (AddN m1)
            :: Instr (MoveN m)
            :: CloseLoop
            :: rest
            when n1 = -1 && m1 = 1 && n <> 0 && m = -n ->
            (* [ - k</> + k>/< ] TransferN to left/right *)
            optimize (Instr (TransferN n) :: acc) rest
          | Instr (MoveN n)
            :: Instr (AddN m1)
            :: Instr (MoveN m)
            :: Instr (AddN n1)
            :: CloseLoop
            :: rest
            when m1 = 1 && n1 = -1 && n <> 0 && m = -n ->
            (* [ k</> + k>/< - ] TransferN to left/right *)
            optimize (Instr (TransferN n) :: acc) rest
          | _ -> optimize (OpenLoop :: acc) tl))
    | Instr (MoveN d1) :: Instr (AddN n) :: Instr (MoveN d2) :: rest when d1 = -d2 ->
      (* k>/<; k+/-; k</>; *)
      optimize (Instr (AddAt (d1, n)) :: acc) rest
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
         let new_tail = if adj = 0 then tl else Instr (MoveN adj) :: tl in
         optimize (Instr (ClearN (d * count, false)) :: acc) new_tail
       | _ -> optimize (Instr (ClearN (d * count, true)) :: acc) rest')
    | Instr Clear :: Instr (AddN n) :: rest -> optimize (Instr (SetConst n) :: acc) rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  fixed_point (fun instrs -> optimize [] instrs) instructions
;;

let resolve_jumps intermediates =
  let open Result in
  let open Hashtbl in
  let add_jump_checked jump_table stack index instr =
    match instr with
    | OpenLoop -> Ok (index :: stack)
    | CloseLoop ->
      (match stack with
       | hd :: rest ->
         set jump_table ~key:hd ~data:index;
         set jump_table ~key:index ~data:hd;
         Ok rest
       | [] -> Error (UnmatchedClosingBracket index))
    | _ -> Ok stack
  in
  let build_jump_table jump_table =
    match
      List.foldi intermediates ~init:(Ok []) ~f:(fun i acc instr ->
        acc >>= fun stack -> add_jump_checked jump_table stack i instr)
    with
    | Ok [] -> Ok jump_table
    | Ok (hd :: _) -> Error (UnmatchedOpeningBracket hd)
    | Error _ as err -> err
  in
  let map_intermediate_to_instr jump_table =
    intermediates
    |> List.mapi ~f:(fun index -> function
      | OpenLoop ->
        let target = find_exn jump_table index in
        Jz (target - index + 1)
      | CloseLoop ->
        let target = find_exn jump_table index in
        Jnz (target - index + 1)
      | Instr real_instr -> real_instr)
  in
  create (module Int) |> build_jump_table >>| map_intermediate_to_instr
;;

(* another step in the pipeline to validate instruction immediates such as ClearN withing -128...127 *)

let full_pass source =
  source |> parse_sequence |> fuse_std_ops |> optimize_patterns |> resolve_jumps
;;
