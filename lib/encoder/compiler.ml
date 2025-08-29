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

let fuse_std_ops instructions =
  let open Instruction in
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

let optimize_patterns instructions =
  let open Instruction in
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
        if Int.( = ) cur_off 0 && seen_dec
        then (
          let nz = List.filter pairs ~f:(fun (_, c) -> c <> 0) in
          match nz with
          | [] -> None
          | [ (_, 1) ] -> None
          | _ -> Some (nz, rest))
        else None
      | Instr (MoveN d) :: tl -> collect pairs seen_dec (cur_off + d) tl
      | Instr (AddN k) :: tl ->
        if Int.( = ) cur_off 0
        then if k = -1 && not seen_dec then collect pairs true cur_off tl else None
        else collect (add_or_update pairs cur_off k) seen_dec cur_off tl
      | _ :: _ -> None
    in
    function
    | Instr (AddN _) :: _ as tl -> collect [] false 0 tl
    | Instr (MoveN _) :: _ as tl -> collect [] false 0 tl
    | CloseLoop :: _ -> None
    | _ -> None
  in
  let rec optimize acc = function
    | [] -> List.rev acc
    | OpenLoop :: tl ->
      (match try_multransfer tl with
       | Some (pairs, rest) -> optimize (Instr (MulTransfer pairs) :: acc) rest
       | None ->
         (match tl with
          | OpenLoop :: OpenLoop :: CloseLoop :: CloseLoop :: CloseLoop :: rest ->
            optimize (Instr Call :: acc) rest
          | CloseLoop :: rest | OpenLoop :: CloseLoop :: CloseLoop :: rest ->
            optimize (Instr Hang :: acc) rest
          | Instr (AddN 1) :: CloseLoop :: rest -> optimize (Instr SetZero :: acc) rest
          | Instr (AddN -1) :: CloseLoop :: rest -> optimize (Instr SetZero :: acc) rest
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
      (* k>k+k< *)
      optimize (Instr (AddAt (d1, n)) :: acc) rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  fixed_point (fun instrs -> optimize [] instrs) instructions
;;

let optimize_single_ops instructions =
  let rec optimize acc = function
    | [] -> List.rev acc
    | Instr (AddN 1) :: rest -> optimize (Instr Add1 :: acc) rest
    | Instr (AddN -1) :: rest -> optimize (Instr Sub1 :: acc) rest
    | Instr (MoveN 1) :: rest -> optimize (Instr Move1R :: acc) rest
    | Instr (MoveN -1) :: rest -> optimize (Instr Move1L :: acc) rest
    | Instr (TransferN 1) :: rest -> optimize (Instr Transfer1R :: acc) rest
    | Instr (TransferN -1) :: rest -> optimize (Instr Transfer1L :: acc) rest
    | Instr (ScanN 1) :: rest -> optimize (Instr Scan1R :: acc) rest
    | Instr (ScanN -1) :: rest -> optimize (Instr Scan1L :: acc) rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  optimize [] instructions
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
  |> fuse_std_ops
  |> optimize_patterns
  |> optimize_single_ops
  |> bind_instruction_offsets
  |> resolve_jumps
  >>= encode_to_bytes
  >>| combine_instruction_bytes
;;
