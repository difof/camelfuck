open Core

type intermediate_instr =
  | Instr of Instruction.t
  | OpenLoop
  | CloseLoop

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

(* apply peephole/micro optimizations on intermediate tokens (preserving loops) *)
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

let resolve_jumps instructions =
  let jump_table = Hashtbl.create (module Int) in
  let bracket_stack = ref [] in
  List.iteri instructions ~f:(fun i instr ->
    match instr with
    | OpenLoop -> bracket_stack := i :: !bracket_stack
    | CloseLoop ->
      (match !bracket_stack with
       | open_i :: rest ->
         bracket_stack := rest;
         Hashtbl.set jump_table ~key:open_i ~data:i;
         Hashtbl.set jump_table ~key:i ~data:open_i
       | [] -> failwith "Unmatched closing bracket")
    | _ -> ());
  if not (List.is_empty !bracket_stack) then failwith "Unmatched opening bracket(s)";
  (* convert to final instructions with relative jumps *)
  List.mapi instructions ~f:(fun i instr ->
    match instr with
    | OpenLoop ->
      let target = Hashtbl.find_exn jump_table i in
      Instruction.Jz (target - i)
    | CloseLoop ->
      let target = Hashtbl.find_exn jump_table i in
      Instruction.Jnz (target - i)
    | Instr real_instr -> real_instr)
;;

let encode_to_bytes instructions =
  List.fold instructions ~init:[] ~f:(fun acc instr ->
    match Instruction.encode instr with
    | Ok bytes -> bytes :: acc
    | Error error ->
      failwith (Format.asprintf "Encoding error: %a" Instruction.pp_error error))
  |> List.rev
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
  source
  |> parse_sequence
  |> pattern_optimize
  |> optimize_instructions
  |> resolve_jumps
  |> encode_to_bytes
  |> combine_instruction_bytes
;;
