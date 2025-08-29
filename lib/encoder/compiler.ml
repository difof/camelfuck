open Core

type intermediate_instr =
  | Instr of Instruction.t
  | OpenLoop
  | CloseLoop

type intermediate_instr_w_offset = IntInstrWOffset of intermediate_instr * int

type error =
  | UnmatchedClosingBracket of int
  | UnmatchedOpeningBracket of int
  | EncodingError of Instruction.error

let pp_error fmt = function
  | UnmatchedClosingBracket i ->
    Format.fprintf fmt "Unmatched closing bracket ']' at instruction %d" i
  | UnmatchedOpeningBracket i ->
    Format.fprintf fmt "Unmatched opening bracket '[' at instruction %d" i
  | EncodingError err -> Instruction.pp_error fmt err
;;

let pp_intermediate_instr fmt = function
  | Instr i -> Format.fprintf fmt "Instr(%a)" Instruction.pp_t i
  | OpenLoop -> Format.fprintf fmt "OpenLoop"
  | CloseLoop -> Format.fprintf fmt "CloseLoop"
;;

let pp_intermediate_instr_w_offset fmt = function
  | IntInstrWOffset (instr, off) ->
    Format.fprintf fmt "%a@%d" pp_intermediate_instr instr off
;;

let parse_sequence source =
  let open Instruction in
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
    let chunks = chunkify cap count [] |> List.map ~f:(fun n -> Instr (instr n)) in
    List.fold chunks ~init:acc ~f:(fun acc i -> i :: acc), next_pos
  in
  let rec parse acc pos =
    if pos >= String.length source
    then List.rev acc
    else (
      match source.[pos] with
      | '+' ->
        let acc, next_pos = accumulate_chunks acc pos '+' (fun n -> AddN n) in
        parse acc next_pos
      | '-' ->
        let acc, next_pos = accumulate_chunks acc pos '-' (fun n -> AddN (-n)) in
        parse acc next_pos
      | '>' ->
        let acc, next_pos = accumulate_chunks acc pos '>' (fun n -> MoveN n) in
        parse acc next_pos
      | '<' ->
        let acc, next_pos = accumulate_chunks acc pos '<' (fun n -> MoveN (-n)) in
        parse acc next_pos
      | '.' -> parse (Instr Out :: acc) (pos + 1)
      | ',' -> parse (Instr In :: acc) (pos + 1)
      | '[' -> parse (OpenLoop :: acc) (pos + 1)
      | ']' -> parse (CloseLoop :: acc) (pos + 1)
      | _ -> parse acc (pos + 1))
  in
  parse [] 0
;;

let rec fixed_point f x =
  let x' = f x in
  if Poly.( = ) x x' then x else fixed_point f x'
;;

(* apply micro optimizations on intermediate tokens *)
let optimize_instructions instructions =
  let open Instruction in
  let rec chunkify acc sum i_multiple i_neg i_pos =
    let v_min = -128 in
    let v_max = 127 in
    match sum with
    | 0 -> acc
    | 1 -> i_pos :: acc
    | -1 -> i_neg :: acc
    | v when v > v_max ->
      chunkify (i_multiple v_max :: acc) (v - v_max) i_multiple i_neg i_pos
    | v when v < v_min ->
      chunkify (i_multiple v_min :: acc) (v - v_min) i_multiple i_neg i_pos
    | v -> i_multiple v :: acc
  in
  let rec optimize acc = function
    | [] -> List.rev acc
    | Instr (AddN 0) :: rest | Instr (MoveN 0) :: rest ->
      (* this should never happen but we use it anyways *)
      optimize acc rest
    | Instr (AddN n) :: Instr (AddN m) :: rest ->
      let acc =
        chunkify acc (n + m) (fun v -> Instr (AddN v)) (Instr Sub1) (Instr Add1)
      in
      optimize acc rest
    | Instr (MoveN n) :: Instr (MoveN m) :: rest ->
      let acc =
        chunkify acc (n + m) (fun v -> Instr (MoveN v)) (Instr Move1L) (Instr Move1R)
      in
      optimize acc rest
    | Instr Move1L :: Instr Move1R :: rest | Instr Move1R :: Instr Move1L :: rest ->
      (* right then left with equal magnitude cancels out, requires running again *)
      optimize acc rest
    | Instr Add1 :: Instr Sub1 :: rest | Instr Sub1 :: Instr Add1 :: rest ->
      (* +n followed by -n cancels out, requires running again *)
      optimize acc rest
    | Instr (AddN 1) :: rest -> optimize (Instr Add1 :: acc) rest
    | Instr (AddN -1) :: rest -> optimize (Instr Sub1 :: acc) rest
    | Instr (MoveN 1) :: rest -> optimize (Instr Move1R :: acc) rest
    | Instr (MoveN -1) :: rest -> optimize (Instr Move1L :: acc) rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  (* iterate optimization to a fixed point (as many times as needed until nothing changes) *)
  fixed_point (fun instrs -> optimize [] instrs) instructions
;;

(* detect common patterns structurally on loops and replace with specialized instructions *)
let optimize_pattern instructions =
  let open Instruction in
  let rec optimize acc = function
    | [] -> List.rev acc
    | OpenLoop :: (Instr Add1 | Instr Sub1) :: CloseLoop :: rest ->
      (* optimized [-] and [+] -> SetZero *)
      optimize (Instr SetZero :: acc) rest
    | (Instr Add1 | Instr Sub1 | Instr (AddN _)) :: Instr SetZero :: rest ->
      (* dead before SetZero *)
      optimize (Instr SetZero :: acc) rest
    | OpenLoop :: Instr Move1R :: CloseLoop :: rest ->
      (* [>] *)
      optimize (Instr Scan1R :: acc) rest
    | OpenLoop :: Instr Move1L :: CloseLoop :: rest ->
      (* [<] *)
      optimize (Instr Scan1L :: acc) rest
    | OpenLoop :: Instr (MoveN n) :: CloseLoop :: rest ->
      (* [k</k>] *)
      optimize (Instr (ScanN n) :: acc) rest
    | OpenLoop
      :: Instr Sub1
      :: Instr Move1R
      :: Instr Add1
      :: Instr Move1L
      :: CloseLoop
      :: rest
    | OpenLoop
      :: Instr Move1R
      :: Instr Add1
      :: Instr Move1L
      :: Instr Sub1
      :: CloseLoop
      :: rest ->
      (* optimized [>+<-] or [->+<] -> Transfer1R *)
      optimize (Instr Transfer1R :: acc) rest
    | OpenLoop
      :: Instr Sub1
      :: Instr Move1L
      :: Instr Add1
      :: Instr Move1R
      :: CloseLoop
      :: rest
    | OpenLoop
      :: Instr Move1L
      :: Instr Add1
      :: Instr Move1R
      :: Instr Sub1
      :: CloseLoop
      :: rest ->
      (* optimized [<+>-] or [-<+>] -> Transfer1L *)
      optimize (Instr Transfer1L :: acc) rest
    | OpenLoop
      :: Instr Sub1
      :: Instr (MoveN n)
      :: Instr Add1
      :: Instr (MoveN m)
      :: CloseLoop
      :: rest
    | OpenLoop
      :: Instr (MoveN n)
      :: Instr Add1
      :: Instr (MoveN m)
      :: Instr Sub1
      :: CloseLoop
      :: rest
      when n <> 0 && m = -n ->
      (* [k</>+k>/<-] or [-k</>+k>/<] TransferN to left/right *)
      optimize (Instr (TransferN n) :: acc) rest
    | Instr (MoveN d1) :: Instr Add1 :: Instr (MoveN d2) :: rest when d1 = -d2 ->
      (* k>+k< *)
      optimize (Instr (AddAt (d1, 1)) :: acc) rest
    | Instr (MoveN d1) :: Instr Sub1 :: Instr (MoveN d2) :: rest when d1 = -d2 ->
      (* k>-k< *)
      optimize (Instr (AddAt (d1, -1)) :: acc) rest
    | Instr (MoveN d1) :: Instr (AddN n) :: Instr (MoveN d2) :: rest when d1 = -d2 ->
      (* k>k+k< *)
      optimize (Instr (AddAt (d1, n)) :: acc) rest
    | OpenLoop :: OpenLoop :: OpenLoop :: CloseLoop :: CloseLoop :: CloseLoop :: rest ->
      (* [[[]]] pattern: runtime Call extension *)
      optimize (Instr Call :: acc) rest
    | OpenLoop :: OpenLoop :: CloseLoop :: CloseLoop :: rest
    | OpenLoop :: CloseLoop :: rest ->
      (* [] and [[]] are infinite loops, we hang. [[[[]]]] and more we don't give a fuck. *)
      optimize (Instr Hang :: acc) rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  fixed_point (fun instrs -> optimize [] instrs) instructions
;;

let bind_instruction_offsets instructions =
  instructions
  |> List.fold ~init:([], 0) ~f:(fun (prev_instructions, offset) instr ->
    let size =
      match instr with
      | OpenLoop | CloseLoop -> 5
      | Instr inner -> Instruction.size inner
    in
    IntInstrWOffset (instr, offset) :: prev_instructions, offset + size)
  |> fst
  |> List.rev
;;

let resolve_jumps instructions_w_offset =
  let open Result in
  let open Hashtbl in
  let add_jump_checked jump_table stack instr offset =
    match instr with
    | OpenLoop -> Ok (offset :: stack)
    | CloseLoop ->
      (match stack with
       | hd :: rest ->
         set jump_table ~key:hd ~data:offset;
         set jump_table ~key:offset ~data:hd;
         Ok rest
       | [] -> Error (UnmatchedClosingBracket offset))
    | _ -> Ok stack
  in
  let build_jump_table jump_table =
    match
      List.fold
        instructions_w_offset
        ~init:(Ok [])
        ~f:(fun acc (IntInstrWOffset (instr, offset)) ->
          acc >>= fun stack -> add_jump_checked jump_table stack instr offset)
    with
    | Ok [] -> Ok jump_table
    | Ok (hd :: _) -> Error (UnmatchedOpeningBracket hd)
    | Error _ as err -> err
  in
  let map_intermediate_to_instr jump_table =
    instructions_w_offset
    |> List.map ~f:(fun (IntInstrWOffset (instr, offset)) ->
      match instr with
      | OpenLoop ->
        let target = find_exn jump_table offset in
        let jump_to = target + 5 in
        Instruction.Jz (jump_to - offset)
      | CloseLoop ->
        let target = find_exn jump_table offset in
        let jump_to = target + 5 in
        Instruction.Jnz (jump_to - offset)
      | Instr real_instr -> real_instr)
  in
  create (module Int) |> build_jump_table >>| map_intermediate_to_instr
;;

let encode_to_bytes instructions =
  let open Result in
  List.fold instructions ~init:(Ok []) ~f:(fun acc instr ->
    acc
    >>= fun acc ->
    match Instruction.encode instr with
    | Ok bytes -> Ok (bytes :: acc)
    | Error err -> Error (EncodingError err))
  >>| List.rev
;;

let combine_instruction_bytes instr_bytes_list =
  let code_size =
    instr_bytes_list |> List.fold ~init:0 ~f:(fun acc instr -> Bytes.length instr + acc)
  in
  let code = Bytes.make code_size '\000' in
  instr_bytes_list
  |> List.fold ~init:0 ~f:(fun pos instr ->
    let len = Bytes.length instr in
    Bytes.blit ~src:instr ~src_pos:0 ~dst:code ~dst_pos:pos ~len;
    pos + len)
  |> ignore;
  code
;;

let compile source =
  let open Result in
  source
  |> parse_sequence
  |> optimize_instructions
  |> optimize_pattern
  |> bind_instruction_offsets
  |> resolve_jumps
  >>= encode_to_bytes
  >>| combine_instruction_bytes
;;
