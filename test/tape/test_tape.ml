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
  move_exn tape 5;
  Alcotest.(check int) "move to position 5" 5 (logical_pos tape);
  set tape 10;
  move_exn tape 3;
  Alcotest.(check int) "move to position 8" 8 (logical_pos tape);
  Alcotest.(check int) "new position has value 0" 0 (get tape);
  move_exn tape (-3);
  Alcotest.(check int) "move back to position 5" 5 (logical_pos tape);
  Alcotest.(check int) "position 5 still has value 10" 10 (get tape)
;;

let test_move_negative () =
  let tape = create ~max_size:4096 256 in
  move_exn tape (-5);
  Alcotest.(check int) "move to position -5" (-5) (logical_pos tape);
  set tape 20;
  Alcotest.(check int) "set value at negative position" 20 (get tape);
  move_exn tape (-3);
  Alcotest.(check int) "move to position -8" (-8) (logical_pos tape);
  Alcotest.(check int) "new negative position has value 0" 0 (get tape);
  move_exn tape 3;
  Alcotest.(check int) "move back to position -5" (-5) (logical_pos tape);
  Alcotest.(check int) "position -5 still has value 20" 20 (get tape)
;;

let test_move_across_zero () =
  let tape = create ~max_size:4096 256 in
  move_exn tape (-5);
  set tape 15;
  move_exn tape 10;
  Alcotest.(check int) "move from -5 to 5" 5 (logical_pos tape);
  set tape 25;
  move_exn tape (-10);
  Alcotest.(check int) "move from 5 to -5" (-5) (logical_pos tape);
  Alcotest.(check int) "position -5 still has value 15" 15 (get tape)
;;

let test_large_moves () =
  let tape = create ~max_size:4096 256 in
  move_exn tape 550;
  Alcotest.(check int) "move to position 550" 550 (logical_pos tape);
  set tape 100;
  Alcotest.(check int) "set value at position 550" 100 (get tape);
  move_exn tape (-1000);
  Alcotest.(check int) "move to position -450" (-450) (logical_pos tape);
  set tape 200;
  Alcotest.(check int) "set value at position -450" 200 (get tape);
  move_exn tape 600;
  Alcotest.(check int) "move to position 150" 150 (logical_pos tape);
  move_exn tape 400;
  Alcotest.(check int) "move to position 550" 550 (logical_pos tape);
  Alcotest.(check int) "position 550 still has value 100" 100 (get tape)
;;

let test_reallocation () =
  let tape = create ~max_size:4096 256 in
  let positions = [ 100; 200; 300; 400; 500 ] in
  List.iteri
    (fun i position ->
       move_exn tape (position - logical_pos tape);
       set tape (i + 1))
    positions;
  List.iteri
    (fun i position ->
       move_exn tape (position - logical_pos tape);
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
  move_exn tape 1;
  Alcotest.(check int) "moved to position 1" 1 (logical_pos tape);
  set tape 43;
  Alcotest.(check int) "value at position 1" 43 (get tape);
  move_exn tape (-2);
  Alcotest.(check int) "moved to position -1" (-1) (logical_pos tape);
  set tape 44;
  Alcotest.(check int) "value at position -1" 44 (get tape);
  move_exn tape 1;
  Alcotest.(check int) "back to position 0" 0 (logical_pos tape);
  Alcotest.(check int) "position 0 still has value 42" 42 (get tape);
  move_exn tape 1;
  Alcotest.(check int) "position 1 still has value 43" 43 (get tape);
  move_exn tape (-2);
  Alcotest.(check int) "position -1 still has value 44" 44 (get tape)
;;

let test_offset_set_add () =
  let tape = create ~max_size:4096 256 in
  set tape 7;
  set_at_offset_exn tape 2 123;
  Alcotest.(check int) "origin unchanged" 7 (get tape);
  move_exn tape 2;
  Alcotest.(check int) "set_at_offset writes at +2" 123 (get tape);
  move_exn tape (-3);
  Alcotest.(check int) "at -1 before set" 0 (get tape);
  set_at_offset_exn tape 0 7;
  set_at_offset_exn tape 1 0;
  set_at_offset_exn tape (-1) 50;
  move_exn tape (-1);
  Alcotest.(check int) "set_at_offset writes at -1" 50 (get tape)
;;

let test_offset_add_at_offset () =
  let tape = create ~max_size:4096 256 in
  add_at_offset_exn tape 1 10;
  add_at_offset_exn tape 1 5;
  move_exn tape 1;
  Alcotest.(check int) "add_at_offset accumulates" 15 (get tape);
  add_at_offset_exn tape 0 300;
  Alcotest.(check int) "add_at_offset wraps at current cell" 59 (get tape)
;;

let test_multransfer () =
  let tape = create ~max_size:4096 256 in
  set tape 3;
  Tape.multransfer_exn tape [ 1, 1; 3, 1; 4, 2 ];
  Alcotest.(check int) "origin zeroed" 0 (get tape);
  move_exn tape 1;
  Alcotest.(check int) "+1 has 3" 3 (get tape);
  move_exn tape 2;
  Alcotest.(check int) "+3 has 3" 3 (get tape);
  move_exn tape 1;
  Alcotest.(check int) "+4 has 6" 6 (get tape);
  move_exn tape (-4);
  set tape 0;
  Tape.multransfer_exn tape [ -1, 5 ];
  move_exn tape (-1);
  Alcotest.(check int) "no-op when source is zero" 0 (get tape)
;;

let test_mulclear_forward () =
  let tape = create ~max_size:4096 256 in
  (* seed values at +1..+4 *)
  set_at_offset_exn tape 1 10;
  set_at_offset_exn tape 2 11;
  set_at_offset_exn tape 3 12;
  set_at_offset_exn tape 4 13;
  move_exn tape 1;
  Tape.mulclear_exn tape 3;
  (* +1..+3 should be zero; +4 unaffected *)
  Alcotest.(check int) "+1 cleared" 0 (get tape);
  move_exn tape 1;
  Alcotest.(check int) "+2 cleared" 0 (get tape);
  move_exn tape 1;
  Alcotest.(check int) "+3 cleared" 0 (get tape);
  move_exn tape 1;
  Alcotest.(check int) "+4 intact" 13 (get tape);
  (* pointer should remain at +4 due to moves in assertions; reset to origin for cleanliness *)
  move_exn tape (-4)
;;

let test_mulclear_backward () =
  let tape = create ~max_size:4096 256 in
  (* seed values at -1..-3 *)
  set_at_offset_exn tape (-1) 21;
  set_at_offset_exn tape (-2) 22;
  set_at_offset_exn tape (-3) 23;
  move_exn tape (-1);
  Tape.mulclear_exn tape (-2);
  Alcotest.(check int) "-1 cleared" 0 (get tape);
  move_exn tape (-1);
  Alcotest.(check int) "-2 cleared" 0 (get tape);
  move_exn tape (-1);
  Alcotest.(check int) "-3 intact" 23 (get tape);
  move_exn tape 3
;;

let test_mulclear_zero_delta () =
  let tape = create ~max_size:4096 256 in
  set_at_offset_exn tape 1 7;
  Tape.mulclear_exn tape 0;
  move_exn tape 1;
  Alcotest.(check int) "zero delta is no-op" 7 (get tape);
  move_exn tape (-1)
;;

let test_mulclear_realloc_early_return_right () =
  (* small max_size to force realloc on clearing beyond current len edge *)
  let tape = create ~max_size:4096 32 in
  (* place value near right edge of current buffer to trigger realloc when clearing to +N *)
  move_exn tape ((len tape / 2) - 2);
  set tape 99;
  move_exn tape (-logical_pos tape);
  Tape.mulclear_exn tape (logical_pos tape + 10);
  (* After realloc, we should return early; verify that the far cell is zero (it is newly allocated) *)
  move_exn tape (logical_pos tape + 10);
  Alcotest.(check int) "far cell zero after realloc" 0 (get tape);
  move_exn tape (-logical_pos tape)
;;

let test_mulclear_realloc_early_return_left () =
  let tape = create ~max_size:4096 32 in
  (* move near left, then attempt large negative clear to force realloc *)
  move_exn tape (-((len tape / 2) - 2));
  set tape 55;
  move_exn tape (-logical_pos tape);
  Tape.mulclear_exn tape (-(abs (logical_pos tape) + 10));
  move_exn tape (-(abs (logical_pos tape) + 10));
  Alcotest.(check int) "far left cell zero after realloc" 0 (get tape);
  move_exn tape (-logical_pos tape)
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
    ; ( "specialized"
      , [ test_case "multransfer" `Quick test_multransfer
        ; test_case "mulclear forward" `Quick test_mulclear_forward
        ; test_case "mulclear backward" `Quick test_mulclear_backward
        ; test_case "mulclear zero delta" `Quick test_mulclear_zero_delta
        ; test_case
            "mulclear realloc right"
            `Quick
            test_mulclear_realloc_early_return_right
        ; test_case "mulclear realloc left" `Quick test_mulclear_realloc_early_return_left
        ] )
    ]
;;
