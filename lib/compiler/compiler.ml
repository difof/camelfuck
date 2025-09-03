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
  source |> parse_sequence |> fuse_std_ops |> Pattern_optimizer.run |> resolve_jumps
;;
