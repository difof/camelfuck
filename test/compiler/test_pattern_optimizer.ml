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

(* MultiTransfer: typical right/left, zero-filter, and non-return-to-origin (no match) *)
let test_multransfer_basic () =
  expect
    "multransfer right basic"
    "+[->+>>+>++<<<<]-"
    [ Instr (Add 1); Instr (MultiTransfer [ 1, 1; 3, 1; 4, 2 ]); Instr (Add (-1)) ];
  expect
    "multransfer left basic"
    "+[<+<<+<++>>>>-]-"
    [ Instr (Add 1); Instr (MultiTransfer [ -4, 2; -3, 1; -1, 1 ]); Instr (Add (-1)) ]
;;

let test_multransfer_zero_and_nonreturn () =
  (* zero coefficients filtered, single remaining pair with coeff 2 is ok *)
  expect
    "multransfer zero filtered"
    "+[->++<>>++<<>--<]-"
    [ Instr (Add 1); Instr (MultiTransfer [ 2, 2 ]); Instr (Add (-1)) ];
  (* not returning to origin -> no multransfer; sequence remains as-is *)
  expect "multransfer not origin -> unchanged" "+[->+>]-" (parse_sequence "+[->+>]-")
;;

(* Call: [[[]]] *)
let test_call () =
  expect "call" "+[[[]]]-" [ Instr (Add 1); Instr Call; Instr (Add (-1)) ]
;;

(* Hang: [] and [[]] *)
let test_hang () =
  expect "hang empty" "+[]-" [ Instr (Add 1); Instr Hang; Instr (Add (-1)) ];
  expect "hang nested" "+[[]]-" [ Instr (Add 1); Instr Hang; Instr (Add (-1)) ]
;;

(* Clear: [+] and [-] *)
let test_clear () =
  expect "clear plus" "+[+]>" [ Instr (Add 1); Instr Clear; Instr (Move 1) ];
  expect "clear minus" "+[-]>" [ Instr (Add 1); Instr Clear; Instr (Move 1) ]
;;

(* ScanStride: [>] and [<] *)
let test_scann () =
  expect "scan right" "+[>]-" [ Instr (Add 1); Instr (ScanStride 1); Instr (Add (-1)) ];
  expect "scan left" "+[<]-" [ Instr (Add 1); Instr (ScanStride (-1)); Instr (Add (-1)) ]
;;

(* TransferStride: two variants and directions *)
let test_transfern () =
  expect
    "transfer right 1"
    "+[- > + <]-"
    [ Instr (Add 1); Instr (TransferStride 1); Instr (Add (-1)) ];
  expect
    "transfer right 2nd form"
    "+[>+<-]-"
    [ Instr (Add 1); Instr (TransferStride 1); Instr (Add (-1)) ];
  expect
    "transfer left 1"
    "+[- < + >]-"
    [ Instr (Add 1); Instr (TransferStride (-1)); Instr (Add (-1)) ];
  expect
    "transfer left 2nd form"
    "+[<+>-]-"
    [ Instr (Add 1); Instr (TransferStride (-1)); Instr (Add (-1)) ]
;;

(* AddAt: exact cancel and overshoot cases *)
let test_addat () =
  (* exact cancel: >> + << => AddAt(2,1) *)
  expect "addat exact" ">>+<<" [ Instr (AddAt (2, 1)) ];
  (* first overshoots: >>> + << => AddAt(2,1); Move 1 (leftover before return) *)
  expect "addat first overshoot" ">>>+<<" [ Instr (Move 1); Instr (AddAt (2, 1)) ];
  (* second overshoots: >> + <<< => Move (-1); AddAt(2,1) (leftover after) *)
  expect "addat second overshoot" ">>+<<<" [ Instr (AddAt (2, 1)); Instr (Move (-1)) ]
;;

(* TrailAdd: accumulate multiple Move/Add pairs *)
let test_trailadd () =
  (* net move: >>+>++ => TrailAdd[(2,1);(3,2)] *)
  expect "trailadd net move" ">>+>++" [ Instr (TrailAdd [ 2, 1; 3, 2 ]); Instr (Move 3) ];
  (* zero net move: >+<++ => TrailAdd[(0,2);(1,1)] *)
  expect "trailadd zero net" ">+<++" [ Instr (TrailAdd [ 0, 2; 1, 1 ]) ]
;;

(* ClearCells: chained clears with unit steps; with and without tail back-move folding *)
let test_clearn () =
  expect
    "clearn forward move"
    "+[-]>[-]-"
    [ Instr (Add 1); Instr (ClearCells 2); Instr (Move 1); Instr (Add (-1)) ];
  expect
    "clearn backward move"
    "+[-]<[-]-"
    [ Instr (Add 1); Instr (ClearCells (-2)); Instr (Move (-1)); Instr (Add (-1)) ];
  expect
    "clearn forward fold"
    "+[-]>[-]>[-]<<-"
    [ Instr (Add 1); Instr (ClearCells 3); Instr (Add (-1)) ];
  expect
    "clearn backward fold"
    "+[-]<[-]<[-]>>-"
    [ Instr (Add 1); Instr (ClearCells (-3)); Instr (Add (-1)) ];
  expect
    "clearn forward move remaining"
    "+[-]>[-]>[-]<<<-"
    [ Instr (Add 1); Instr (ClearCells 3); Instr (Move (-1)); Instr (Add (-1)) ]
;;

(* SetConst: Clear followed by Add n *)
let test_setconst () = expect "setconst" "+[-]++" [ Instr (Add 1); Instr (SetConst 2) ]

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
    ; "trailadd", [ test_case "trailadd" `Quick test_trailadd ]
    ; "clearn", [ test_case "clearn" `Quick test_clearn ]
    ; "setconst", [ test_case "setconst" `Quick test_setconst ]
    ; "noop", [ test_case "noop" `Quick test_noop_preserve ]
    ]
;;
