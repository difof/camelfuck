let compile_exn s =
  match Encoder.Compiler.compile s with
  | Ok b -> b
  | Error _ -> failwith "compile error"
;;

let make_vm ?io ?memory code = Runtime.Vm.create ?io ?memory code
let run_vm vm = Runtime.Vm.run vm
let get_tape vm = vm.Runtime.Vm.memory

let expect_cell name vm idx expected =
  let mem = get_tape vm in
  let logical_origin = Tape.logical_pos mem in
  Tape.move_exn mem (idx - logical_origin);
  let actual = Tape.get mem in
  Tape.move_exn mem (logical_origin - idx);
  Alcotest.(check int) name expected actual
;;

let test_add_and_move () =
  (* Brainfuck: +++>++ moves value 3 at cell0, value 2 at cell1 *)
  let src = "+++>++" in
  let code = compile_exn src in
  let vm = make_vm code in
  let vm = run_vm vm in
  expect_cell "cell0 is 3" vm 0 3;
  expect_cell "cell1 is 2" vm 1 2
;;

let test_loop_transfer () =
  (* Brainfuck: [>+<-] performs a transfer from cell0 to cell1 *)
  let src = "+++[>+<-]" in
  let code = compile_exn src in
  let vm = make_vm code in
  let vm = run_vm vm in
  expect_cell "cell0 becomes 0" vm 0 0;
  expect_cell "cell1 becomes 3" vm 1 3
;;

let test_setzero () =
  let src = "+++[-]" in
  let code = compile_exn src in
  let vm = make_vm code in
  let vm = run_vm vm in
  expect_cell "cell0 zeroed" vm 0 0
;;

let test_hello_progression () =
  (* ensure compiled program runs and leaves specific memory and pointer position *)
  let src =
    "+++++++++++[>++++++>+++++++++>++++++++>++++>+++>+<<<<<<-]>+++\n"
    ^ "+++.>++.+++++++..+++.>>.>-.<<-.<.+++.------.--------.>>>+.>-."
  in
  let code = compile_exn src in
  let memory = Tape.create 256 in
  let vm = make_vm ~memory code in
  let vm = run_vm vm in
  let memory = get_tape vm in
  (* assert pointer at 6 *)
  Alcotest.(check int) "pointer at 6" 6 (Tape.logical_pos memory);
  (* read first 7 bytes from index 0 using blit *)
  let original_pos = Tape.logical_pos memory in
  Tape.move_exn memory (-original_pos);
  let buf = Bytes.make 7 '\000' in
  Tape.blit_out_exn memory buf 7;
  (* restore pointer position *)
  Tape.move_exn memory original_pos;
  let to_list b =
    let rec loop acc i =
      if i = Bytes.length b
      then List.rev acc
      else loop (Bytes.get_uint8 b i :: acc) (i + 1)
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
