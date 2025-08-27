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
  (* 300 '-' should be split into -127 + -127 + -46 preserving sign via AddN *)
  expect_parse
    "minus capped to 127"
    (make '-' 300)
    [ Instr (AddN (-127)); Instr (AddN (-127)); Instr (AddN (-46)) ];
  (* 300 '>' should be split into 127 + 127 + 46 *)
  expect_parse
    "move right capped to 127"
    (make '>' 300)
    [ Instr (MoveN 127); Instr (MoveN 127); Instr (MoveN 46) ];
  (* 300 '<' should be split into -127 + -127 + -46 *)
  expect_parse
    "move left capped to 127"
    (make '<' 300)
    [ Instr (MoveN (-127)); Instr (MoveN (-127)); Instr (MoveN (-46)) ]
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
  let actual = parse_sequence source |> map_offsets in
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
  let inter = parse_sequence source |> map_offsets in
  match resolve_jumps inter with
  | Ok actual ->
    Alcotest.(check (list string))
      name
      (List.map show_instr expected)
      (List.map show_instr actual)
  | Error err -> Alcotest.failf "expected Ok, got Error: %s" (show_error err)
;;

let expect_resolve_error name source expected_err_s =
  let inter = parse_sequence source |> map_offsets in
  match resolve_jumps inter with
  | Ok _ -> Alcotest.failf "%s: expected Error, got Ok" name
  | Error err -> Alcotest.(check string) name expected_err_s (show_error err)
;;

let test_resolve_empty () = expect_resolve "empty loop" "[]" [ Jz 5; Jnz (-5) ]

let test_resolve_with_body () =
  expect_resolve "loop with body" "[+-]" [ Jz 9; AddN 1; AddN (-1); Jnz (-9) ]
;;

let test_resolve_nested () =
  expect_resolve "nested loops" "[[]]" [ Jz 15; Jz 5; Jnz (-5); Jnz (-15) ]
;;

let test_resolve_mixed () =
  expect_resolve
    "mixed with body around"
    "+[>+<-]-"
    [ AddN 1; Jz 13; MoveN 1; AddN 1; MoveN (-1); AddN (-1); Jnz (-13); AddN (-1) ]
;;

let test_resolve_unmatched_close () =
  expect_resolve_error "unmatched close" "]" "UnmatchedClosingBracket"
;;

let test_resolve_unmatched_open () =
  expect_resolve_error "unmatched open" "[" "UnmatchedOpeningBracket"
;;

(* structural pattern optimization tests *)
let expect_pattern name source expected =
  let actual = parse_sequence source |> optimize_instructions |> pattern_optimize in
  let expected_s = List.map show_intermediate expected in
  let actual_s = List.map show_intermediate actual in
  Alcotest.(check (list string)) name expected_s actual_s
;;

let test_pattern_setzero () = expect_pattern "setzero" "[-]" [ Instr SetZero ]
let test_pattern_copy () = expect_pattern "transferr" "[>+<-]" [ Instr TransferR ]
let test_pattern_call () = expect_pattern "call" "[[[]]]" [ Instr Call ]

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
    ; ( "map_offsets"
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
    ; ( "pattern_optimize"
      , [ test_case "setzero" `Quick test_pattern_setzero
        ; test_case "transferr" `Quick test_pattern_copy
        ; test_case "call" `Quick test_pattern_call
        ] )
    ]
;;
