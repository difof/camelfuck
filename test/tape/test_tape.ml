open Tape

let test_create () =
  let tape = create ~max_size:4096 256 in
  Alcotest.(check int) "initial position" 0 (logical_pos tape);
  Alcotest.(check int) "initial value" 0 (get tape)
;;

let test_set_get () =
  let tape = create ~max_size:4096 256 in
  set tape 42;
  Alcotest.(check int) "set and get value" 42 (get tape);
  set tape 256;
  Alcotest.(check int) "value wraps at 256" 0 (get tape);
  set tape 257;
  Alcotest.(check int) "value wraps at 257" 1 (get tape)
;;

let test_add () =
  let tape = create ~max_size:4096 256 in
  add tape 10;
  Alcotest.(check int) "add increments" 10 (get tape);
  add tape 5;
  Alcotest.(check int) "add accumulates" 15 (get tape);
  add tape 300;
  Alcotest.(check int) "add wraps" 59 (get tape)
;;

let test_move_positive () =
  let tape = create ~max_size:4096 256 in
  (match move tape 5 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move to position 5" 5 (logical_pos tape);
  set tape 10;
  (match move tape 3 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move to position 8" 8 (logical_pos tape);
  Alcotest.(check int) "new position has value 0" 0 (get tape);
  (match move tape (-3) with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move back to position 5" 5 (logical_pos tape);
  Alcotest.(check int) "position 5 still has value 10" 10 (get tape)
;;

let test_move_negative () =
  let tape = create ~max_size:4096 256 in
  (match move tape (-5) with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move to position -5" (-5) (logical_pos tape);
  set tape 20;
  Alcotest.(check int) "set value at negative position" 20 (get tape);
  (match move tape (-3) with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move to position -8" (-8) (logical_pos tape);
  Alcotest.(check int) "new negative position has value 0" 0 (get tape);
  (match move tape 3 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move back to position -5" (-5) (logical_pos tape);
  Alcotest.(check int) "position -5 still has value 20" 20 (get tape)
;;

let test_move_across_zero () =
  let tape = create ~max_size:4096 256 in
  (match move tape (-5) with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  set tape 15;
  (match move tape 10 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move from -5 to 5" 5 (logical_pos tape);
  set tape 25;
  (match move tape (-10) with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move from 5 to -5" (-5) (logical_pos tape);
  Alcotest.(check int) "position -5 still has value 15" 15 (get tape)
;;

let test_large_moves () =
  let tape = create ~max_size:4096 256 in
  (match move tape 550 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move to position 550" 550 (logical_pos tape);
  set tape 100;
  Alcotest.(check int) "set value at position 550" 100 (get tape);
  (match move tape (-1000) with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move to position -450" (-450) (logical_pos tape);
  set tape 200;
  Alcotest.(check int) "set value at position -450" 200 (get tape);
  (match move tape 600 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move to position 150" 150 (logical_pos tape);
  (match move tape 400 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "move to position 550" 550 (logical_pos tape);
  Alcotest.(check int) "position 550 still has value 100" 100 (get tape)
;;

let test_reallocation () =
  let tape = create ~max_size:4096 256 in
  let positions = [ 100; 200; 300; 400; 500 ] in
  List.iteri
    (fun i position ->
       (match move tape (position - logical_pos tape) with
        | Ok () -> ()
        | Error _ -> Alcotest.fail "move should succeed");
       set tape (i + 1))
    positions;
  List.iteri
    (fun i position ->
       (match move tape (position - logical_pos tape) with
        | Ok () -> ()
        | Error _ -> Alcotest.fail "move should succeed");
       Alcotest.(check int)
         (Printf.sprintf "value at position %d" position)
         (i + 1)
         (get tape))
    positions
;;

let test_edge_positions () =
  let tape = create ~max_size:4096 256 in
  Alcotest.(check int) "initial position is 0" 0 (logical_pos tape);
  set tape 42;
  Alcotest.(check int) "value at position 0" 42 (get tape);
  (match move tape 1 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "moved to position 1" 1 (logical_pos tape);
  set tape 43;
  Alcotest.(check int) "value at position 1" 43 (get tape);
  (match move tape (-2) with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "moved to position -1" (-1) (logical_pos tape);
  set tape 44;
  Alcotest.(check int) "value at position -1" 44 (get tape);
  (match move tape 1 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "back to position 0" 0 (logical_pos tape);
  Alcotest.(check int) "position 0 still has value 42" 42 (get tape);
  (match move tape 1 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "position 1 still has value 43" 43 (get tape);
  (match move tape (-2) with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "position -1 still has value 44" 44 (get tape)
;;

let test_offset_set_add () =
  let tape = create ~max_size:4096 256 in
  set tape 7;
  set_at_offset_exn tape 2 123;
  Alcotest.(check int) "origin unchanged" 7 (get tape);
  (match move tape 2 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "set_at_offset writes at +2" 123 (get tape);
  (match move tape (-3) with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "at -1 before set" 0 (get tape);
  set_at_offset_exn tape 0 7;
  set_at_offset_exn tape 1 0;
  set_at_offset_exn tape (-1) 50;
  (match move tape (-1) with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "set_at_offset writes at -1" 50 (get tape)
;;

let test_offset_add_at_offset () =
  let tape = create ~max_size:4096 256 in
  add_at_offset_exn tape 1 10;
  add_at_offset_exn tape 1 5;
  (match move tape 1 with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "move should succeed");
  Alcotest.(check int) "add_at_offset accumulates" 15 (get tape);
  add_at_offset_exn tape 0 300;
  Alcotest.(check int) "add_at_offset wraps at current cell" 59 (get tape)
;;

let test_error_cases () =
  let tape = create ~max_size:512 10 in
  match move tape 5000 with
  | Ok () -> Alcotest.fail "Expected move to fail due to max allocation"
  | Error (MaxAllocationReached (_, max)) ->
    Alcotest.(check int) "move over allocation max" 512 max
  | Error _ -> Alcotest.fail "Expected MaxAllocationReached error"
;;

let () =
  let open Alcotest in
  run
    "Tape tests"
    [ ( "basic"
      , [ test_case "create" `Quick test_create
        ; test_case "set and get" `Quick test_set_get
        ; test_case "add" `Quick test_add
        ] )
    ; ( "movement"
      , [ test_case "move positive" `Quick test_move_positive
        ; test_case "move negative" `Quick test_move_negative
        ; test_case "move across zero" `Quick test_move_across_zero
        ; test_case "large moves" `Quick test_large_moves
        ; test_case "edge positions (-1, 0, 1)" `Quick test_edge_positions
        ] )
    ; ( "offset ops"
      , [ test_case "set_at_offset and origin" `Quick test_offset_set_add
        ; test_case "add_at_offset" `Quick test_offset_add_at_offset
        ] )
    ; "memory", [ test_case "reallocation" `Quick test_reallocation ]
    ; "errors", [ test_case "error cases" `Quick test_error_cases ]
    ]
;;
