open Encoder.Compiler
open Encoder.Instruction

let show_intermediate = function
  | Instr i -> Format.asprintf "Instr(%a)" pp_t i
  | OpenLoop -> "OpenLoop"
  | CloseLoop -> "CloseLoop"
;;

let expect_parse name source expected =
  let actual = parse_sequence source in
  let expected_s = List.map show_intermediate expected in
  let actual_s = List.map show_intermediate actual in
  Alcotest.(check (list string)) name expected_s actual_s
;;

let test_basic () =
  expect_parse
    "basic tokens"
    "+-<>.,"
    [ Instr (AddN 1)
    ; Instr (AddN (-1))
    ; Instr (MoveN (-1))
    ; Instr (MoveN 1)
    ; Instr Out
    ; Instr In
    ]
;;

let test_counts () =
  expect_parse
    "counted runs"
    "+++-->>><<<"
    [ Instr (AddN 3); Instr (AddN (-2)); Instr (MoveN 3); Instr (MoveN (-3)) ]
;;

let test_counts_capped () =
  let make s n = String.make n s in
  (* 300 '+' should be split into 127 + 127 + 46 *)
  expect_parse
    "plus capped to 127"
    (make '+' 300)
    [ Instr (AddN 127); Instr (AddN 127); Instr (AddN 46) ];
  (* 300 '-' should be split into -128 + -128 + -44 preserving sign via AddN *)
  expect_parse
    "minus capped to 128"
    (make '-' 300)
    [ Instr (AddN (-128)); Instr (AddN (-128)); Instr (AddN (-44)) ];
  (* 300 '>' should be split into 127 + 127 + 46 *)
  expect_parse
    "move right capped to 127"
    (make '>' 300)
    [ Instr (MoveN 127); Instr (MoveN 127); Instr (MoveN 46) ];
  (* 300 '<' should be split into -128 + -128 + -44 *)
  expect_parse
    "move left capped to 128"
    (make '<' 300)
    [ Instr (MoveN (-128)); Instr (MoveN (-128)); Instr (MoveN (-44)) ]
;;

let test_loops () =
  expect_parse "empty loop" "[]" [ OpenLoop; CloseLoop ];
  expect_parse "nested loops" "[[]]" [ OpenLoop; OpenLoop; CloseLoop; CloseLoop ]
;;

let test_ignores () = expect_parse "ignores others" "a b\nc\t+ " [ Instr (AddN 1) ]

(* resolve_jumps tests *)
let show_instr t = Format.asprintf "%a" pp_t t

let show_error = function
  | UnmatchedClosingBracket _ -> "UnmatchedClosingBracket"
  | UnmatchedOpeningBracket _ -> "UnmatchedOpeningBracket"
  | EncodingError _ -> "EncodingError"
;;

let show_offset = function
  | IntInstrWOffset (instr, off) -> Format.asprintf "%s@%d" (show_intermediate instr) off
;;

let expect_offsets name source expected =
  let actual = parse_sequence source |> bind_instruction_offsets in
  let actual_s = List.map show_offset actual in
  let expected_s = List.map show_offset expected in
  Alcotest.(check (list string)) name expected_s actual_s
;;

let test_offsets_empty () =
  expect_offsets
    "offsets for empty loop"
    "[]"
    [ IntInstrWOffset (OpenLoop, 0); IntInstrWOffset (CloseLoop, 5) ]
;;

let test_offsets_mixed () =
  (* "+[>+<-]-." -> offsets:
     addn(1)@0 (2 bytes)
     [@2 (5 bytes)
     moven(1)@7 (2 bytes)
     addn(1)@9 (2 bytes)
     moven(-1)@11 (2 bytes)
     addn(-1)@13 (2 bytes)
     ]@15 (5 bytes)
     addn(-1)@20 (2 bytes)
     out@22 (1 byte)
  *)
  expect_offsets
    "offsets for mixed sequence"
    "+[>+<-]-."
    [ IntInstrWOffset (Instr (AddN 1), 0)
    ; IntInstrWOffset (OpenLoop, 2)
    ; IntInstrWOffset (Instr (MoveN 1), 7)
    ; IntInstrWOffset (Instr (AddN 1), 9)
    ; IntInstrWOffset (Instr (MoveN (-1)), 11)
    ; IntInstrWOffset (Instr (AddN (-1)), 13)
    ; IntInstrWOffset (CloseLoop, 15)
    ; IntInstrWOffset (Instr (AddN (-1)), 20)
    ; IntInstrWOffset (Instr Out, 22)
    ]
;;

let expect_resolve name source expected =
  let inter = parse_sequence source |> bind_instruction_offsets in
  match resolve_jumps inter with
  | Ok actual ->
    Alcotest.(check (list string))
      name
      (List.map show_instr expected)
      (List.map show_instr actual)
  | Error err -> Alcotest.failf "expected Ok, got Error: %s" (show_error err)
;;

let expect_resolve_error name source expected_err_s =
  let inter = parse_sequence source |> bind_instruction_offsets in
  match resolve_jumps inter with
  | Ok _ -> Alcotest.failf "%s: expected Error, got Ok" name
  | Error err -> Alcotest.(check string) name expected_err_s (show_error err)
;;

let test_resolve_empty () = expect_resolve "empty loop" "[]" [ Jz 10; Jnz 0 ]

let test_resolve_with_body () =
  expect_resolve "loop with body" "[+-]" [ Jz 14; AddN 1; AddN (-1); Jnz (-4) ]
;;

let test_resolve_nested () =
  expect_resolve "nested loops" "[[]]" [ Jz 20; Jz 10; Jnz 0; Jnz (-10) ]
;;

let test_resolve_mixed () =
  expect_resolve
    "mixed with body around"
    "+[>+<-]-"
    [ AddN 1; Jz 18; MoveN 1; AddN 1; MoveN (-1); AddN (-1); Jnz (-8); AddN (-1) ]
;;

let test_resolve_unmatched_close () =
  expect_resolve_error "unmatched close" "]" "UnmatchedClosingBracket"
;;

let test_resolve_unmatched_open () =
  expect_resolve_error "unmatched open" "[" "UnmatchedOpeningBracket"
;;

(* structural pattern optimization tests *)
let expect_pattern name source expected =
  let actual = parse_sequence source |> optimize_instructions |> optimize_pattern in
  let expected_s = List.map show_intermediate expected in
  let actual_s = List.map show_intermediate actual in
  Alcotest.(check (list string)) name expected_s actual_s
;;

let test_pattern_setzero () = expect_pattern "setzero" "[-]" [ Instr SetZero ]
let test_pattern_copy () = expect_pattern "transfer1r" "[>+<-]" [ Instr Transfer1R ]
let test_pattern_copy_l () = expect_pattern "transfer1l" "[<+>-]" [ Instr Transfer1L ]
let test_pattern_call () = expect_pattern "call" "[[[]]]" [ Instr Call ]

let test_pattern_transfern_r () =
  (* pattern: [>>+<<-] -> TransferN 2 *)
  expect_pattern "transfern right" "[>>+<<-]" [ Instr (TransferN 2) ]
;;

let test_pattern_transfern_l () =
  (* pattern: [<<+>>-] -> TransferN -2 *)
  expect_pattern "transfern left" "[<<+>>-]" [ Instr (TransferN (-2)) ]
;;

let test_pattern_scan1r () = expect_pattern "scan1r" "[>]" [ Instr Scan1R ]
let test_pattern_scan1l () = expect_pattern "scan1l" "[<]" [ Instr Scan1L ]
let test_pattern_scann_r () = expect_pattern "scann right" "[>>]" [ Instr (ScanN 2) ]
let test_pattern_scann_l () = expect_pattern "scann left" "[<<]" [ Instr (ScanN (-2)) ]

let test_pattern_addat_add1_r () =
  expect_pattern "addat + right" ">>+<<" [ Instr (AddAt (2, 1)) ]
;;

let test_pattern_addat_sub1_r () =
  expect_pattern "addat - right" ">>-<<" [ Instr (AddAt (2, -1)) ]
;;

let test_pattern_addat_addn_r () =
  expect_pattern "addat +n right" ">>+++<<" [ Instr (AddAt (2, 3)) ]
;;

let test_pattern_addat_add1_l () =
  expect_pattern "addat + left" "<<+>>" [ Instr (AddAt (-2, 1)) ]
;;

let test_pattern_addat_addn_l () =
  expect_pattern "addat +n left" "<<--->>" [ Instr (AddAt (-2, -3)) ]
;;

let test_pattern_addat_no_match () =
  (* ">>+>>" should not fold into AddAt since distances don't cancel *)
  let actual = parse_sequence ">>+>>" |> optimize_instructions |> optimize_pattern in
  let expected = [ Instr (MoveN 2); Instr Add1; Instr (MoveN 2) ] in
  let expected_s = List.map show_intermediate expected in
  let actual_s = List.map show_intermediate actual in
  Alcotest.(check (list string)) "addat no match" expected_s actual_s
;;

(* operation folding + encoding tests *)
let bytes_to_list b =
  let len = Bytes.length b in
  let rec loop acc i =
    if i = len then List.rev acc else loop (Bytes.get_uint8 b i :: acc) (i + 1)
  in
  loop [] 0
;;

let expect_compile_bytes name source expected_bytes =
  match compile source with
  | Ok b -> Alcotest.(check (list int)) name expected_bytes (bytes_to_list b)
  | Error _ -> Alcotest.failf "%s: expected Ok, got Error" name
;;

let make c n = String.make n c

let test_fold_add () =
  (* 127 + 127 -> two AddN 127 opcodes *)
  expect_compile_bytes "add 127+127" (make '+' 254) [ 0x01; 0x7F; 0x01; 0x7F ];
  (* -128 + -128 -> two AddN -128 opcodes *)
  expect_compile_bytes "add -128+-128" (make '-' 256) [ 0x01; 0x80; 0x01; 0x80 ];
  (* 90 + 90 -> 127 + 53 remainder after folding *)
  expect_compile_bytes
    "add 90+90 (folded)"
    (make '+' 90 ^ " " ^ make '+' 90)
    [ 0x01; 0x7F; 0x01; 0x35 ];
  (* 100 + -100 -> cancels to empty *)
  expect_compile_bytes "add cancels to zero" (make '+' 100 ^ make '-' 100) []
;;

let test_fold_move () =
  (* 127 + 127 -> two MoveN 127 opcodes *)
  expect_compile_bytes "move 127+127" (make '>' 254) [ 0x02; 0x7F; 0x02; 0x7F ];
  (* -128 + -128 -> two MoveN -128 opcodes *)
  expect_compile_bytes "move -128+-128" (make '<' 256) [ 0x02; 0x80; 0x02; 0x80 ];
  (* 90 + 90 -> 127 + 53 remainder after folding *)
  expect_compile_bytes
    "move 90+90 (folded)"
    (make '>' 90 ^ " " ^ make '>' 90)
    [ 0x02; 0x7F; 0x02; 0x35 ];
  (* 100 + -100 -> cancels to empty *)
  expect_compile_bytes "move cancels to zero" (make '>' 100 ^ make '<' 100) []
;;

let () =
  let open Alcotest in
  run
    "Compiler tests"
    [ ( "parse_sequence"
      , [ test_case "basic" `Quick test_basic
        ; test_case "counts" `Quick test_counts
        ; test_case "counts capped" `Quick test_counts_capped
        ; test_case "loops" `Quick test_loops
        ; test_case "ignores" `Quick test_ignores
        ] )
    ; ( "bind_instruction_offsets"
      , [ test_case "empty" `Quick test_offsets_empty
        ; test_case "mixed" `Quick test_offsets_mixed
        ] )
    ; ( "resolve_jumps"
      , [ test_case "empty" `Quick test_resolve_empty
        ; test_case "with body" `Quick test_resolve_with_body
        ; test_case "nested" `Quick test_resolve_nested
        ; test_case "mixed" `Quick test_resolve_mixed
        ; test_case "unmatched close" `Quick test_resolve_unmatched_close
        ; test_case "unmatched open" `Quick test_resolve_unmatched_open
        ] )
    ; ( "optimize_pattern"
      , [ test_case "setzero" `Quick test_pattern_setzero
        ; test_case "transfer1r" `Quick test_pattern_copy
        ; test_case "transfer1l" `Quick test_pattern_copy_l
        ; test_case "transfern right" `Quick test_pattern_transfern_r
        ; test_case "transfern left" `Quick test_pattern_transfern_l
        ; test_case "scan1r" `Quick test_pattern_scan1r
        ; test_case "scan1l" `Quick test_pattern_scan1l
        ; test_case "scann right" `Quick test_pattern_scann_r
        ; test_case "scann left" `Quick test_pattern_scann_l
        ; test_case "addat + right" `Quick test_pattern_addat_add1_r
        ; test_case "addat - right" `Quick test_pattern_addat_sub1_r
        ; test_case "addat +n right" `Quick test_pattern_addat_addn_r
        ; test_case "addat + left" `Quick test_pattern_addat_add1_l
        ; test_case "addat +n left" `Quick test_pattern_addat_addn_l
        ; test_case "addat no match" `Quick test_pattern_addat_no_match
        ; test_case "call" `Quick test_pattern_call
        ] )
    ; ( "folding"
      , [ test_case "add folding encodes" `Quick test_fold_add
        ; test_case "move folding encodes" `Quick test_fold_move
        ] )
    ]
;;
