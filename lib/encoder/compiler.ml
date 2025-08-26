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

let parse_sequence source =
  (* count consecutive identical characters *)
  let rec count_consecutive pos target_char acc =
    if pos >= String.length source || not (Char.equal source.[pos] target_char)
    then acc, pos
    else count_consecutive (pos + 1) target_char (acc + 1)
  in
  let chunkify count =
    let rec loop remaining acc =
      if remaining <= 0
      then List.rev acc
      else (
        let take = Int.min 255 remaining in
        loop (remaining - take) (take :: acc))
    in
    loop count []
  in
  let accumulate_chunks acc pos target_char instr =
    let count, next_pos = count_consecutive pos target_char 0 in
    let chunks = chunkify count |> List.map ~f:(fun n -> Instr (instr n)) in
    List.fold chunks ~init:acc ~f:(fun acc i -> i :: acc), next_pos
  in
  let rec parse acc pos =
    if pos >= String.length source
    then List.rev acc
    else (
      match source.[pos] with
      | '+' ->
        let acc, next_pos = accumulate_chunks acc pos '+' (fun n -> Instruction.AddN n) in
        parse acc next_pos
      | '-' ->
        let acc, next_pos = accumulate_chunks acc pos '-' (fun n -> Instruction.SubN n) in
        parse acc next_pos
      | '>' ->
        let acc, next_pos =
          accumulate_chunks acc pos '>' (fun n -> Instruction.MoveNR n)
        in
        parse acc next_pos
      | '<' ->
        let acc, next_pos =
          accumulate_chunks acc pos '<' (fun n -> Instruction.MoveNL n)
        in
        parse acc next_pos
      | '.' -> parse (Instr Instruction.Out :: acc) (pos + 1)
      | ',' -> parse (Instr Instruction.In :: acc) (pos + 1)
      | '[' -> parse (OpenLoop :: acc) (pos + 1)
      | ']' -> parse (CloseLoop :: acc) (pos + 1)
      | _ -> parse acc (pos + 1))
  in
  parse [] 0
;;

(* apply micro optimizations on intermediate tokens *)
let optimize_instructions instructions =
  let rec optimize acc = function
    | [] -> List.rev acc
    | Instr (Instruction.AddN 1) :: rest -> optimize (Instr Instruction.Add1 :: acc) rest
    | Instr (Instruction.SubN 1) :: rest -> optimize (Instr Instruction.Sub1 :: acc) rest
    | Instr (Instruction.MoveNR 1) :: rest ->
      optimize (Instr Instruction.Move1R :: acc) rest
    | Instr (Instruction.MoveNL 1) :: rest ->
      optimize (Instr Instruction.Move1L :: acc) rest
    | Instr (Instruction.AddN n) :: Instr (Instruction.SubN m) :: rest when n = m ->
      (* +n followed by -n cancels out *)
      optimize acc rest
    | Instr (Instruction.SubN n) :: Instr (Instruction.AddN m) :: rest when n = m ->
      (* -n followed by +n cancels out *)
      optimize acc rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  optimize [] instructions
;;

(* detect common patterns structurally on loops and replace with specialized instructions *)
let pattern_optimize instructions =
  let rec optimize acc = function
    | [] -> List.rev acc
    (* optimized [-] -> setzero *)
    | OpenLoop :: Instr (Instruction.SubN 1) :: CloseLoop :: rest ->
      optimize (Instr Instruction.SetZero :: acc) rest
    (* optimized [>+<-] -> copy *)
    | OpenLoop
      :: Instr (Instruction.MoveNR 1)
      :: Instr (Instruction.AddN 1)
      :: Instr (Instruction.MoveNL 1)
      :: Instr (Instruction.SubN 1)
      :: CloseLoop
      :: rest -> optimize (Instr Instruction.Copy :: acc) rest
    (* [[[]]] pattern: runtime CALL extension *)
    | OpenLoop :: OpenLoop :: OpenLoop :: CloseLoop :: CloseLoop :: CloseLoop :: rest ->
      optimize (Instr Instruction.Call :: acc) rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  optimize [] instructions
;;

let map_offsets instructions =
  (* accumulate instructions and last offset *)
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
  let fill_jump_table jt =
    match
      (* build jump table via stack *)
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
               set jt ~key:hd ~data:offset;
               set jt ~key:offset ~data:hd;
               Ok rest
             | [] -> Error (UnmatchedClosingBracket offset))
          | _ -> Ok stack)
    with
    | Ok [] -> Ok jt
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
  |> resolve_jumps
  >>= encode_to_bytes
  >>| combine_instruction_bytes
;;
