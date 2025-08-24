open Encoder.Compiler
open Encoder.Instruction

let show_intermediate = function
  | Instr i -> Format.asprintf "Instr(%a)" pp_t i
  | OpenLoop -> "OpenLoop"
  | CloseLoop -> "CloseLoop"
;;

let expect_parse name source expected =
  let actual = parse_sequence source 0 [] in
  let expected_s = List.map show_intermediate expected in
  let actual_s = List.map show_intermediate actual in
  Alcotest.(check (list string)) name expected_s actual_s
;;

let test_basic () =
  expect_parse
    "basic tokens"
    "+-<>.,"
    [ Instr (Add 1)
    ; Instr (Sub 1)
    ; Instr (Move (-1))
    ; Instr (Move 1)
    ; Instr Out
    ; Instr In
    ]
;;

let test_counts () =
  expect_parse
    "counted runs"
    "+++-->>><<<"
    [ Instr (Add 3); Instr (Sub 2); Instr (Move 3); Instr (Move (-3)) ]
;;

let test_loops () =
  expect_parse "empty loop" "[]" [ OpenLoop; CloseLoop ];
  expect_parse "nested loops" "[[]]" [ OpenLoop; OpenLoop; CloseLoop; CloseLoop ]
;;

let test_ignores () = expect_parse "ignores others" "a b\nc\t+ " [ Instr (Add 1) ]

let () =
  let open Alcotest in
  run
    "Compiler parse_sequence tests"
    [ ( "parse_sequence"
      , [ test_case "basic" `Quick test_basic
        ; test_case "counts" `Quick test_counts
        ; test_case "loops" `Quick test_loops
        ; test_case "ignores" `Quick test_ignores
        ] )
    ]
;;
