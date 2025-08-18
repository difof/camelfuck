open Tape.ArrayTape

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
  (* Test moving beyond initial page size (256) *)
  move tape 550;
  Alcotest.(check int) "move to position 550" 550 (pos tape);
  set tape 100;
  Alcotest.(check int) "set value at position 550" 100 (get tape);
  move tape (-1000);
  Alcotest.(check int) "move to position -450" (-450) (pos tape);
  set tape 200;
  Alcotest.(check int) "set value at position -450" 200 (get tape);
  move tape 600;
  Alcotest.(check int) "move to position 150" 150 (pos tape);
  move tape 400;
  Alcotest.(check int) "move to position 550" 550 (pos tape);
  Alcotest.(check int) "position 550 still has value 100" 100 (get tape)
;;

let test_reallocation () =
  let tape = create () in
  (* Test that reallocation preserves existing values *)
  let positions = [ 100; 200; 300; 400; 500 ] in
  List.iteri
    (fun i position ->
       move tape (position - pos tape);
       set tape (i + 1))
    positions;
  (* Verify all values are preserved *)
  List.iteri
    (fun i position ->
       move tape (position - pos tape);
       Alcotest.(check int)
         (Printf.sprintf "value at position %d" position)
         (i + 1)
         (get tape))
    positions
;;

let () =
  let open Alcotest in
  run
    "Arraytape tests"
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
    ; "memory", [ test_case "reallocation" `Quick test_reallocation ]
    ]
;;
