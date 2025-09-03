open Core
open Isa
open Compiler

let show_intr t = Format.asprintf "%a" pp_intr t

let expect name source expected =
  let actual = parse_sequence source |> fuse_std_ops |> Pattern_optimizer.run in
  let s_expected = List.map ~f:show_intr expected in
  let s_actual = List.map ~f:show_intr actual in
  Alcotest.(check (list string)) name s_expected s_actual
;;

(* MulTransfer: typical right/left, zero-filter, and non-return-to-origin (no match) *)
let test_multransfer_basic () =
  expect
    "multransfer right basic"
    "+[->+>>+>++<<<<]-"
    [ Instr (AddN 1); Instr (MulTransfer [ 1, 1; 3, 1; 4, 2 ]); Instr (AddN (-1)) ];
  expect
    "multransfer left basic"
    "+[<+<<+<++>>>>-]-"
    [ Instr (AddN 1); Instr (MulTransfer [ -4, 2; -3, 1; -1, 1 ]); Instr (AddN (-1)) ]
;;

let test_multransfer_zero_and_nonreturn () =
  (* zero coefficients filtered, single remaining pair with coeff 2 is ok *)
  expect
    "multransfer zero filtered"
    "+[->++<>>++<<>--<]-"
    [ Instr (AddN 1); Instr (MulTransfer [ 2, 2 ]); Instr (AddN (-1)) ];
  (* not returning to origin -> no multransfer; sequence remains as-is *)
  expect "multransfer not origin -> unchanged" "+[->+>]-" (parse_sequence "+[->+>]-")
;;

(* Call: [[[]]] *)
let test_call () =
  expect "call" "+[[[]]]-" [ Instr (AddN 1); Instr Call; Instr (AddN (-1)) ]
;;

(* Hang: [] and [[]] *)
let test_hang () =
  expect "hang empty" "+[]-" [ Instr (AddN 1); Instr Hang; Instr (AddN (-1)) ];
  expect "hang nested" "+[[]]-" [ Instr (AddN 1); Instr Hang; Instr (AddN (-1)) ]
;;

(* Clear: [+] and [-] *)
let test_clear () =
  expect "clear plus" "+[+]>" [ Instr (AddN 1); Instr Clear; Instr (MoveN 1) ];
  expect "clear minus" "+[-]>" [ Instr (AddN 1); Instr Clear; Instr (MoveN 1) ]
;;

(* ScanN: [>] and [<] *)
let test_scann () =
  expect "scan right" "+[>]-" [ Instr (AddN 1); Instr (ScanN 1); Instr (AddN (-1)) ];
  expect "scan left" "+[<]-" [ Instr (AddN 1); Instr (ScanN (-1)); Instr (AddN (-1)) ]
;;

(* TransferN: two variants and directions *)
let test_transfern () =
  expect
    "transfer right 1"
    "+[- > + <]-"
    [ Instr (AddN 1); Instr (TransferN 1); Instr (AddN (-1)) ];
  expect
    "transfer right 2nd form"
    "+[>+<-]-"
    [ Instr (AddN 1); Instr (TransferN 1); Instr (AddN (-1)) ];
  expect
    "transfer left 1"
    "+[- < + >]-"
    [ Instr (AddN 1); Instr (TransferN (-1)); Instr (AddN (-1)) ];
  expect
    "transfer left 2nd form"
    "+[<+>-]-"
    [ Instr (AddN 1); Instr (TransferN (-1)); Instr (AddN (-1)) ]
;;

(* AddAt: exact cancel and overshoot cases *)
let test_addat () =
  (* exact cancel: >> + << => AddAt(2,1) *)
  expect "addat exact" ">>+<<" [ Instr (AddAt (2, 1)) ];
  (* first overshoots: >>> + << => AddAt(2,1); MoveN 1 (leftover before return) *)
  expect "addat first overshoot" ">>>+<<" [ Instr (MoveN 1); Instr (AddAt (2, 1)) ];
  (* second overshoots: >> + <<< => MoveN (-1); AddAt(2,1) (leftover after) *)
  expect "addat second overshoot" ">>+<<<" [ Instr (AddAt (2, 1)); Instr (MoveN (-1)) ]
;;

(* ClearN: chained clears with unit steps; with and without tail back-move folding *)
let test_clearn () =
  expect
    "clearn forward true"
    "+[-]>[-]-"
    [ Instr (AddN 1); Instr (ClearN (2, true)); Instr (AddN (-1)) ];
  expect
    "clearn backward true"
    "+[-]<[-]-"
    [ Instr (AddN 1); Instr (ClearN (-2, true)); Instr (AddN (-1)) ];
  expect
    "clearn forward false fold"
    "+[-]>[-]>[-]<<-"
    [ Instr (AddN 1); Instr (ClearN (3, false)); Instr (AddN (-1)) ];
  expect
    "clearn backward false fold"
    "+[-]<[-]<[-]>>-"
    [ Instr (AddN 1); Instr (ClearN (-3, false)); Instr (AddN (-1)) ]
;;

(* SetConst: Clear followed by AddN n *)
let test_setconst () = expect "setconst" "+[-]++" [ Instr (AddN 1); Instr (SetConst 2) ]

(* No-op: sequences that should remain unchanged *)
let test_noop_preserve () =
  let src = ">.+<" in
  expect "noop preserve misc" src (parse_sequence src)
;;

let () =
  let open Alcotest in
  run
    "Pattern optimizer tests"
    [ ( "multransfer"
      , [ test_case "basic" `Quick test_multransfer_basic
        ; test_case "zero and nonreturn" `Quick test_multransfer_zero_and_nonreturn
        ] )
    ; "call", [ test_case "call" `Quick test_call ]
    ; "hang", [ test_case "hang" `Quick test_hang ]
    ; "clear", [ test_case "clear" `Quick test_clear ]
    ; "scan", [ test_case "scan" `Quick test_scann ]
    ; "transfer", [ test_case "transfer" `Quick test_transfern ]
    ; "addat", [ test_case "addat" `Quick test_addat ]
    ; "clearn", [ test_case "clearn" `Quick test_clearn ]
    ; "setconst", [ test_case "setconst" `Quick test_setconst ]
    ; "noop", [ test_case "noop" `Quick test_noop_preserve ]
    ]
;;
