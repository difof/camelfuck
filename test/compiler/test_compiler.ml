open Core
open Compiler
open Isa

let show_intermediate = function
  | Instr i -> Format.asprintf "Instr(%a)" pp_t i
  | OpenLoop -> "OpenLoop"
  | CloseLoop -> "CloseLoop"
;;

let expect_parse name source expected =
  let actual = parse_sequence source in
  let expected_s = List.map ~f:show_intermediate expected in
  let actual_s = List.map ~f:show_intermediate actual in
  Alcotest.(check (list string)) name expected_s actual_s
;;

let test_basic () =
  expect_parse
    "basic tokens"
    "+-<>.,[]"
    [ Instr (Add 1)
    ; Instr (Add (-1))
    ; Instr (Move (-1))
    ; Instr (Move 1)
    ; Instr Out
    ; Instr In
    ; OpenLoop
    ; CloseLoop
    ]
;;

let test_counts_and_caps () =
  let make c n = String.make n c in
  expect_parse "plus fused" "++++" [ Instr (Add 4) ];
  expect_parse "minus fused" "----" [ Instr (Add (-4)) ];
  expect_parse "right fused" ">>>>" [ Instr (Move 4) ];
  expect_parse "left fused" "<<<<" [ Instr (Move (-4)) ];
  (* capping *)
  let s = make '+' 300 in
  expect_parse "plus capped" s [ Instr (Add 127); Instr (Add 127); Instr (Add 46) ];
  let s = make '-' 300 in
  expect_parse
    "minus capped"
    s
    [ Instr (Add (-128)); Instr (Add (-128)); Instr (Add (-44)) ];
  let s = make '>' 300 in
  expect_parse "right capped" s [ Instr (Move 127); Instr (Move 127); Instr (Move 46) ];
  let s = make '<' 300 in
  expect_parse
    "left capped"
    s
    [ Instr (Move (-128)); Instr (Move (-128)); Instr (Move (-44)) ]
;;

let test_loops () =
  expect_parse "empty loop" "[]" [ OpenLoop; CloseLoop ];
  expect_parse "nested loops" "[[]]" [ OpenLoop; OpenLoop; CloseLoop; CloseLoop ]
;;

let test_ignore_noise () =
  expect_parse "ignores spaces and others" "a b\n\t+" [ Instr (Add 1) ]
;;

let show_instr t = Format.asprintf "%a" pp_t t

let show_error = function
  | UnmatchedClosingBracket _ -> "UnmatchedClosingBracket"
  | UnmatchedOpeningBracket _ -> "UnmatchedOpeningBracket"
;;

let expect_resolve name source expected =
  match parse_sequence source |> resolve_jumps with
  | Ok actual ->
    Alcotest.(check (list string))
      name
      (List.map ~f:show_instr expected)
      (List.map ~f:show_instr actual)
  | Error err -> Alcotest.failf "%s: expected Ok, got Error %a" name Compiler.pp_error err
;;

let expect_resolve_error name source expected_err =
  match parse_sequence source |> resolve_jumps with
  | Ok _ -> Alcotest.failf "%s: expected Error, got Ok" name
  | Error err -> Alcotest.(check string) name expected_err (show_error err)
;;

let expect_resolve_exact name source expected =
  match parse_sequence source |> resolve_jumps with
  | Ok actual ->
    Alcotest.(check (list string))
      name
      (List.map ~f:show_instr expected)
      (List.map ~f:show_instr actual)
  | Error err -> Alcotest.failf "%s: expected Ok, got Error %a" name Compiler.pp_error err
;;

let test_resolve_empty_loop () =
  (* intermediates: [ Open, Close ]
     Jz jumps to after Close; Jnz jumps back to Open *)
  expect_resolve "empty loop" "[]" [ Jz 2; Jnz 0 ]
;;

let test_resolve_with_body () =
  (* pattern: [+ -] inside a loop. We only assert relative correctness shape, not exact bytes *)
  match parse_sequence "[+-]" |> resolve_jumps with
  | Error err -> Alcotest.failf "resolve error: %a" Compiler.pp_error err
  | Ok instrs ->
    (* should be: Jz k; Add 1; Add -1; Jnz m *)
    (match instrs with
     | [ Jz _; Add 1; Add -1; Jnz _ ] -> ()
     | _ -> Alcotest.fail "unexpected resolved sequence")
;;

let test_resolve_with_body_exact () =
  expect_resolve_exact "with body exact" "[+-]" [ Jz 4; Add 1; Add (-1); Jnz (-2) ]
;;

let test_resolve_nested () =
  match parse_sequence "[[]]" |> resolve_jumps with
  | Error err -> Alcotest.failf "resolve error: %a" Compiler.pp_error err
  | Ok instrs ->
    (* Expect: outer Jz _, inner Jz _, inner Jnz _, outer Jnz _ *)
    (match instrs with
     | [ Jz _; Jz _; Jnz _; Jnz _ ] -> ()
     | _ -> Alcotest.fail "unexpected nested jumps resolution")
;;

let test_resolve_nested_exact () =
  expect_resolve_exact "nested exact" "[[]]" [ Jz 4; Jz 2; Jnz 0; Jnz (-2) ]
;;

let test_resolve_mixed_exact () =
  expect_resolve_exact
    "mixed exact"
    "+[>+<-]-"
    [ Add 1; Jz 6; Move 1; Add 1; Move (-1); Add (-1); Jnz (-4); Add (-1) ]
;;

let test_resolve_errors () =
  expect_resolve_error "unmatched close" "]" "UnmatchedClosingBracket";
  expect_resolve_error "unmatched open" "[" "UnmatchedOpeningBracket"
;;

let test_fuse_std_ops () =
  let instrs = parse_sequence "+++-->>><<<" |> fuse_std_ops in
  let s = List.map ~f:show_intermediate instrs in
  Alcotest.(check (list string)) "fused" [ "Instr(Add(1))" ] s
;;

let () =
  let open Alcotest in
  run
    "Compiler (refactored) tests"
    [ ( "parse_sequence"
      , [ test_case "basic" `Quick test_basic
        ; test_case "counts and caps" `Quick test_counts_and_caps
        ; test_case "loops" `Quick test_loops
        ; test_case "ignore" `Quick test_ignore_noise
        ] )
    ; ( "resolve_jumps"
      , [ test_case "empty" `Quick test_resolve_empty_loop
        ; test_case "with body" `Quick test_resolve_with_body
        ; test_case "nested" `Quick test_resolve_nested
        ; test_case "errors" `Quick test_resolve_errors
        ; test_case "with body exact" `Quick test_resolve_with_body_exact
        ; test_case "nested exact" `Quick test_resolve_nested_exact
        ; test_case "mixed exact" `Quick test_resolve_mixed_exact
        ] )
    ; "optimize", [ test_case "fuse_std_ops" `Quick test_fuse_std_ops ]
    ]
;;
