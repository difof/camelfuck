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

(* resolve_jumps tests *)
let show_instr t = Format.asprintf "%a" pp_t t

let expect_resolve name source expected =
  let inter = parse_sequence source in
  let actual = resolve_jumps inter in
  Alcotest.(check (list string))
    name
    (List.map show_instr expected)
    (List.map show_instr actual)
;;

let test_resolve_empty () = expect_resolve "empty loop" "[]" [ Jz 1; Jnz (-1) ]

let test_resolve_with_body () =
  expect_resolve "loop with body" "[+-]" [ Jz 3; Add 1; Sub 1; Jnz (-3) ]
;;

let test_resolve_nested () =
  expect_resolve "nested loops" "[[]]" [ Jz 3; Jz 1; Jnz (-1); Jnz (-3) ]
;;

let test_resolve_mixed () =
  expect_resolve
    "mixed with body around"
    "+[>+<-]-"
    [ Add 1; Jz 5; Move 1; Add 1; Move (-1); Sub 1; Jnz (-5); Sub 1 ]
;;

let test_resolve_unmatched_close () =
  Alcotest.check_raises "unmatched close" (Failure "Unmatched closing bracket") (fun () ->
    resolve_jumps [ CloseLoop ] |> ignore)
;;

let test_resolve_unmatched_open () =
  Alcotest.check_raises
    "unmatched open"
    (Failure "Unmatched opening bracket(s)")
    (fun () -> resolve_jumps [ OpenLoop ] |> ignore)
;;

let () =
  let open Alcotest in
  run
    "Compiler tests"
    [ ( "parse_sequence"
      , [ test_case "basic" `Quick test_basic
        ; test_case "counts" `Quick test_counts
        ; test_case "loops" `Quick test_loops
        ; test_case "ignores" `Quick test_ignores
        ] )
    ; ( "resolve_jumps"
      , [ test_case "empty" `Quick test_resolve_empty
        ; test_case "with body" `Quick test_resolve_with_body
        ; test_case "nested" `Quick test_resolve_nested
        ; test_case "mixed" `Quick test_resolve_mixed
        ; test_case "unmatched close" `Quick test_resolve_unmatched_close
        ; test_case "unmatched open" `Quick test_resolve_unmatched_open
        ] )
    ]
;;
