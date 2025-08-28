open Encoder.Instruction

let bytes_to_list b =
  let len = Bytes.length b in
  let rec loop acc i =
    if i = len then List.rev acc else loop (Bytes.get_uint8 b i :: acc) (i + 1)
  in
  loop [] 0
;;

let expect_ok name instr expected_bytes =
  match encode instr with
  | Ok b -> Alcotest.(check (list int)) name expected_bytes (bytes_to_list b)
  | Error _ -> Alcotest.fail (name ^ ": expected Ok, got Error")
;;

let expect_error name instr =
  match encode instr with
  | Ok _ -> Alcotest.fail (name ^ ": expected Error, got Ok")
  | Error _ -> ()
;;

let int32_bytes_le i32 =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 i32;
  [ Bytes.get_uint8 b 0; Bytes.get_uint8 b 1; Bytes.get_uint8 b 2; Bytes.get_uint8 b 3 ]
;;

let test_add () =
  expect_ok "add +5" (AddN 5) [ 0x01; 0x05 ];
  expect_ok "add -1" (AddN (-1)) [ 0x01; 0xFF ];
  expect_ok "add -128" (AddN (-128)) [ 0x01; 0x80 ];
  expect_ok "add 127" (AddN 127) [ 0x01; 0x7F ];
  expect_error "add -129" (AddN (-129));
  expect_error "add 128" (AddN 128)
;;

let test_move () =
  expect_ok "move +3" (MoveN 3) [ 0x02; 0x03 ];
  expect_ok "move -1" (MoveN (-1)) [ 0x02; 0xFF ];
  expect_ok "move -128" (MoveN (-128)) [ 0x02; 0x80 ];
  expect_ok "move 127" (MoveN 127) [ 0x02; 0x7F ];
  expect_error "move -129" (MoveN (-129));
  expect_error "move 128" (MoveN 128)
;;

let test_jz () =
  expect_ok "jz 0" (Jz 0) (0x03 :: int32_bytes_le Int32.zero);
  expect_ok "jz 1" (Jz 1) (0x03 :: int32_bytes_le Int32.one);
  expect_ok "jz -1" (Jz (-1)) (0x03 :: int32_bytes_le Int32.minus_one)
;;

let test_jnz () =
  expect_ok "jnz 42" (Jnz 42) (0x04 :: int32_bytes_le (Int32.of_int 42));
  expect_ok "jnz -42" (Jnz (-42)) (0x04 :: int32_bytes_le (Int32.of_int (-42)))
;;

let test_noarg () =
  expect_ok "in" In [ 0x05 ];
  expect_ok "out" Out [ 0x06 ];
  expect_ok "call" Call [ 0x07 ];
  expect_ok "setzero" SetZero [ 0x08 ];
  expect_ok "transferr" TransferR [ 0x09 ];
  expect_ok "transferl" TransferL [ 0x0E ];
  expect_ok "add1" Add1 [ 0x0A ];
  expect_ok "sub1" Sub1 [ 0x0B ];
  expect_ok "move1r" Move1R [ 0x0C ];
  expect_ok "move1l" Move1L [ 0x0D ]
;;

let () =
  let open Alcotest in
  run
    "Instruction encoding tests"
    [ "byte ops", [ test_case "add" `Quick test_add; test_case "move" `Quick test_move ]
    ; "jumps", [ test_case "jz" `Quick test_jz; test_case "jnz" `Quick test_jnz ]
    ; "no-arg ops", [ test_case "no-arg encodings" `Quick test_noarg ]
    ]
;;
