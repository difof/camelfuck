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
  let rec parse pos acc =
    if pos >= String.length source
    then List.rev acc
    else (
      match source.[pos] with
      | '+' ->
        let count, next_pos = count_consecutive pos '+' 0 in
        let chunks =
          chunkify count |> List.map ~f:(fun n -> Instr (Instruction.AddN n))
        in
        let acc = List.fold chunks ~init:acc ~f:(fun acc i -> i :: acc) in
        parse next_pos acc
      | '-' ->
        let count, next_pos = count_consecutive pos '-' 0 in
        let chunks =
          chunkify count |> List.map ~f:(fun n -> Instr (Instruction.SubN n))
        in
        let acc = List.fold chunks ~init:acc ~f:(fun acc i -> i :: acc) in
        parse next_pos acc
      | '>' ->
        let count, next_pos = count_consecutive pos '>' 0 in
        let chunks =
          chunkify count |> List.map ~f:(fun n -> Instr (Instruction.MoveNR n))
        in
        let acc = List.fold chunks ~init:acc ~f:(fun acc i -> i :: acc) in
        parse next_pos acc
      | '<' ->
        let count, next_pos = count_consecutive pos '<' 0 in
        let chunks =
          chunkify count |> List.map ~f:(fun n -> Instr (Instruction.MoveNL n))
        in
        let acc = List.fold chunks ~init:acc ~f:(fun acc i -> i :: acc) in
        parse next_pos acc
      | '.' -> parse (pos + 1) (Instr Instruction.Out :: acc)
      | ',' -> parse (pos + 1) (Instr Instruction.In :: acc)
      | '[' -> parse (pos + 1) (OpenLoop :: acc)
      | ']' -> parse (pos + 1) (CloseLoop :: acc)
      | _ -> parse (pos + 1) acc)
  in
  parse 0 []
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

(* apply peephole optimizations *)
let optimize_instructions instructions =
  let rec optimize acc = function
    | [] -> List.rev acc
    | Instruction.AddN 1 :: rest -> optimize (Instruction.Add1 :: acc) rest
    | Instruction.SubN 1 :: rest -> optimize (Instruction.Sub1 :: acc) rest
    | Instruction.MoveNR 1 :: rest -> optimize (Instruction.Move1R :: acc) rest
    | Instruction.MoveNL 1 :: rest -> optimize (Instruction.Move1L :: acc) rest
    | Instruction.AddN n :: Instruction.SubN m :: rest when n = m ->
      (* +n followed by -n cancels out *)
      optimize acc rest
    | Instruction.SubN n :: Instruction.AddN m :: rest when n = m ->
      (* -n followed by +n cancels out *)
      optimize acc rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  optimize [] instructions
;;

(* detect common patterns and replace with specialized instructions *)
let pattern_optimize instructions =
  let rec optimize acc = function
    | [] -> List.rev acc
    | Instruction.Jz 2 :: Instruction.Sub1 :: Instruction.Jnz -2 :: rest ->
      (* optimized [-] *)
      optimize (Instruction.SetZero :: acc) rest
    | Instruction.Jz 5
      :: Instruction.Move1R
      :: Instruction.Add1
      :: Instruction.Move1L
      :: Instruction.Sub1
      :: Instruction.Jnz -5
      :: rest ->
      (* optimized [>+<-] *)
      optimize (Instruction.Copy :: acc) rest
    | Instruction.Jz 5
      :: Instruction.Jz 3
      :: Instruction.Jz 1
      :: Instruction.Jnz -1
      :: Instruction.Jnz -3
      :: Instruction.Jnz -5
      :: rest ->
      (* [[[]]] pattern: runtime CALL extension *)
      optimize (Instruction.Call :: acc) rest
    | instr :: rest -> optimize (instr :: acc) rest
  in
  optimize [] instructions
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
  |> resolve_jumps
  |> optimize_instructions
  |> pattern_optimize
  |> encode_to_bytes
  |> combine_instruction_bytes
;;
