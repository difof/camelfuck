open Runtime.Vm

let compile_exn s =
  match Compiler.full_pass s with
  | Ok b -> b
  | Error _ -> failwith "compile error"
;;

let make_vm ?io ?(memory = Tape.create 256) code = create ?io ~memory code, memory

let run_vm vm =
  match run vm with
  | Ok _ -> ()
  | Error err -> Alcotest.failf "Test failed with error: %a" pp_error err
;;

let expect_cell name mem idx expected =
  let open Tape in
  let logical_origin = logical_pos mem in
  move_exn mem (idx - logical_origin);
  let actual = get mem in
  move_exn mem (logical_origin - idx);
  Alcotest.(check int) name expected actual
;;

let test_add_and_move () =
  (* Brainfuck: +++>++ moves value 3 at cell0, value 2 at cell1 *)
  let src = "+++>++" in
  let code = compile_exn src in
  let vm, mem = make_vm code in
  run_vm vm;
  expect_cell "cell0 is 3" mem 0 3;
  expect_cell "cell1 is 2" mem 1 2
;;

let test_loop_transfer () =
  (* Brainfuck: [>+<-] performs a transfer from cell0 to cell1 *)
  let src = "+++[>+<-]" in
  let code = compile_exn src in
  let vm, mem = make_vm code in
  run_vm vm;
  expect_cell "cell0 becomes 0" mem 0 0;
  expect_cell "cell1 becomes 3" mem 1 3
;;

let test_setzero () =
  let src = "+++[-]" in
  let code = compile_exn src in
  let vm, mem = make_vm code in
  run_vm vm;
  expect_cell "cell0 zeroed" mem 0 0
;;

let test_hello_progression () =
  (* ensure compiled program runs and leaves specific memory and pointer position *)
  let src =
    "+++++++++++[>++++++>+++++++++>++++++++>++++>+++>+<<<<<<-]>+++\n"
    ^ "+++.>++.+++++++..+++.>>.>-.<<-.<.+++.------.--------.>>>+.>-."
  in
  let code = compile_exn src in
  let mem = Tape.create 256 in
  let vm, _ = make_vm ~memory:mem code in
  run_vm vm;
  (* assert pointer at 6 *)
  Alcotest.(check int) "pointer at 6" 6 (Tape.logical_pos mem);
  (* read first 7 bytes from index 0 using blit *)
  let original_pos = Tape.logical_pos mem in
  Tape.move_exn mem (-original_pos);
  let buf = Array.make 7 0 in
  Tape.blit_out_exn mem buf 7;
  (* restore pointer position *)
  Tape.move_exn mem original_pos;
  let to_list b =
    let rec loop acc i =
      if i = Array.length b then List.rev acc else loop (b.(i) :: acc) (i + 1)
    in
    loop [] 0
  in
  Alcotest.(check (list int))
    "hello memory"
    [ 0x00; 0x48; 0x64; 0x57; 0x2C; 0x21; 0x0A ]
    (to_list buf)
;;

let () =
  let open Alcotest in
  run
    "VM tests"
    [ ( "vm"
      , [ test_case "add and move" `Quick test_add_and_move
        ; test_case "loop transfer" `Quick test_loop_transfer
        ; test_case "setzero" `Quick test_setzero
        ; test_case "hello progression" `Quick test_hello_progression
        ] )
    ]
;;
