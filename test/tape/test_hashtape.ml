open Tape.Hashtape

let test_create () =
  let tape = create () in
  Alcotest.(check int) "initial position" 0 (pos tape);
  Alcotest.(check int) "initial value" 0 (get tape)
;;

let test_set_get () =
  let tape = create () in
  set tape 42;
  Alcotest.(check int) "set and get value" 42 (get tape);
  set tape 256;
  Alcotest.(check int) "value wraps at 256" 0 (get tape);
  set tape 257;
  Alcotest.(check int) "value wraps at 257" 1 (get tape)
;;

let test_move_positive () =
  let tape = create () in
  move tape 5;
  Alcotest.(check int) "move to position 5" 5 (pos tape);
  set tape 10;
  move tape 3;
  Alcotest.(check int) "move to position 8" 8 (pos tape);
  Alcotest.(check int) "new position has value 0" 0 (get tape);
  move tape (-3);
  Alcotest.(check int) "move back to position 5" 5 (pos tape);
  Alcotest.(check int) "position 5 still has value 10" 10 (get tape)
;;

let test_move_negative () =
  let tape = create () in
  move tape (-5);
  Alcotest.(check int) "move to position -5" (-5) (pos tape);
  set tape 20;
  Alcotest.(check int) "set value at negative position" 20 (get tape);
  move tape (-3);
  Alcotest.(check int) "move to position -8" (-8) (pos tape);
  Alcotest.(check int) "new negative position has value 0" 0 (get tape);
  move tape 3;
  Alcotest.(check int) "move back to position -5" (-5) (pos tape);
  Alcotest.(check int) "position -5 still has value 20" 20 (get tape)
;;

let test_move_across_zero () =
  let tape = create () in
  move tape (-5);
  set tape 15;
  move tape 10;
  Alcotest.(check int) "move from -5 to 5" 5 (pos tape);
  set tape 25;
  move tape (-10);
  Alcotest.(check int) "move from 5 to -5" (-5) (pos tape);
  Alcotest.(check int) "position -5 still has value 15" 15 (get tape)
;;

let test_large_moves () =
  let tape = create () in
  move tape 1000;
  Alcotest.(check int) "move to position 1000" 1000 (pos tape);
  set tape 100;
  Alcotest.(check int) "set value at position 1000" 100 (get tape);
  move tape (-2000);
  Alcotest.(check int) "move to position -1000" (-1000) (pos tape);
  set tape 200;
  Alcotest.(check int) "set value at position -1000" 200 (get tape);
  move tape 1500;
  Alcotest.(check int) "move to position 500" 500 (pos tape);
  move tape 500;
  Alcotest.(check int) "move to position 1000" 1000 (pos tape);
  Alcotest.(check int) "position 1000 still has value 100" 100 (get tape)
;;

let test_sparse_storage () =
  let tape = create () in
  let positions = [ -10000; -5000; 0; 5000; 10000 ] in
  List.iteri
    (fun i position ->
       move tape (position - pos tape);
       set tape (i + 1))
    positions;
  List.iteri
    (fun i position ->
       move tape (position - pos tape);
       Alcotest.(check int)
         (Printf.sprintf "value at position %d" position)
         (i + 1)
         (get tape))
    positions
;;

let test_uninitialized_cells () =
  let tape = create () in
  let positions = [ -100; -50; 50; 100; 1000; -1000 ] in
  List.iter
    (fun position ->
       move tape (position - pos tape);
       Alcotest.(check int)
         (Printf.sprintf "uninitialized cell at position %d" position)
         0
         (get tape))
    positions
;;

let () =
  let open Alcotest in
  run
    "Hashtape tests"
    [ ( "basic"
      , [ test_case "create" `Quick test_create
        ; test_case "set and get" `Quick test_set_get
        ] )
    ; ( "movement"
      , [ test_case "move positive" `Quick test_move_positive
        ; test_case "move negative" `Quick test_move_negative
        ; test_case "move across zero" `Quick test_move_across_zero
        ; test_case "large moves" `Quick test_large_moves
        ] )
    ; ( "hashtable specific"
      , [ test_case "sparse storage" `Quick test_sparse_storage
        ; test_case "uninitialized cells" `Quick test_uninitialized_cells
        ] )
    ]
;;
