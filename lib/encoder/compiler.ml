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
  let rec chunkify remaining acc =
    if remaining <= 0
    then List.rev acc
    else (
      let take = Int.min 127 remaining in
      chunkify (remaining - take) (take :: acc))
  in
  let accumulate_chunks acc pos target_char instr =
    let count, next_pos = count_consecutive pos target_char 0 in
    let chunks = chunkify count [] |> List.map ~f:(fun n -> Instr (instr n)) in
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

(* apply micro optimizations on intermediate tokens *)
let optimize_instructions instructions =
  let open Instruction in
  let rec optimize acc = function
    | [] -> List.rev acc
    | Instr (AddN 1) :: rest -> optimize (Instr Add1 :: acc) rest
    | Instr (AddN -1) :: rest -> optimize (Instr Sub1 :: acc) rest
    | Instr (MoveN 1) :: rest -> optimize (Instr Move1R :: acc) rest
    | Instr (MoveN -1) :: rest -> optimize (Instr Move1L :: acc) rest
    | Instr (AddN n) :: Instr (AddN m) :: rest when n + m = 0 ->
      (* +n followed by -n cancels out (using signed AddN) *)
      optimize acc rest
    | Instr (MoveN n) :: Instr (MoveN m) :: rest when n + m = 0 ->
      (* right then left with equal magnitude cancels out *)
      optimize acc rest
    | Instr Move1L :: Instr Move1R :: rest | Instr Move1R :: Instr Move1L :: rest ->
      (* right then left with equal magnitude cancels out, requires running again *)
      optimize acc rest
    | Instr Add1 :: Instr Sub1 :: rest | Instr Sub1 :: Instr Add1 :: rest ->
      (* +n followed by -n cancels out, requires running again *)
      optimize acc rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  (* double optimize to cancel out no imm instructions *)
  optimize [] instructions |> optimize []
;;

(* detect common patterns structurally on loops and replace with specialized instructions *)
let pattern_optimize instructions =
  let open Instruction in
  let rec optimize acc = function
    | [] -> List.rev acc
    (* optimized [-] -> setzero *)
    | OpenLoop :: Instr (AddN -1) :: CloseLoop :: rest ->
      optimize (Instr SetZero :: acc) rest
    (* optimized [>+<-] -> copy *)
    | OpenLoop
      :: Instr (MoveN 1)
      :: Instr (AddN 1)
      :: Instr (MoveN -1)
      :: Instr (AddN -1)
      :: CloseLoop
      :: rest -> optimize (Instr Copy :: acc) rest
    (* [[[]]] pattern: runtime CALL extension *)
    | OpenLoop :: OpenLoop :: OpenLoop :: CloseLoop :: CloseLoop :: CloseLoop :: rest ->
      optimize (Instr Call :: acc) rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  optimize [] instructions
;;

let map_offsets instructions =
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
  let fill_jump_table jump_table =
    match
      List.fold
        instructions_w_offset
        ~init:(Ok [])
        ~f:(fun acc (IntInstrWOffset (instr, offset)) ->
          acc
          >>= fun stack ->
          match instr with
          | OpenLoop -> Ok (offset :: stack)
          | CloseLoop ->
            (match stack with
             | hd :: rest ->
               set jump_table ~key:hd ~data:offset;
               set jump_table ~key:offset ~data:hd;
               Ok rest
             | [] -> Error (UnmatchedClosingBracket offset))
          | _ -> Ok stack)
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
        Instruction.Jz (target - offset)
      | CloseLoop ->
        let target = find_exn jump_table offset in
        Instruction.Jnz (target - offset)
      | Instr real_instr -> real_instr)
  in
  create (module Int) |> fill_jump_table >>| map_intermediate_to_instr
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
  |> pattern_optimize
  |> optimize_instructions
  |> map_offsets
  |> resolve_jumps
  >>= encode_to_bytes
  >>| combine_instruction_bytes
;;
