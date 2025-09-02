open Isa

let bytes_to_list b =
  let len = Bytes.length b in
  let rec loop acc i =
    if i = len then List.rev acc else loop (Bytes.get_uint8 b i :: acc) (i + 1)
  in
  loop [] 0
;;

let expect_ok_bytes name instr expected_bytes =
  match encode instr with
  | Ok b -> Alcotest.(check (list int)) name expected_bytes (bytes_to_list b)
  | Error _ -> Alcotest.fail (name ^ ": expected Ok, got Error")
;;

let expect_error name instr =
  match encode instr with
  | Ok _ -> Alcotest.fail (name ^ ": expected Error, got Ok")
  | Error _ -> ()
;;

let int32_bytes_le i =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int i);
  [ Bytes.get_uint8 b 0; Bytes.get_uint8 b 1; Bytes.get_uint8 b 2; Bytes.get_uint8 b 3 ]
;;

let test_to_char_and_size () =
  (* opcode mapping and byte_size checks *)
  Alcotest.(check char) "Hang opcode" (Char.chr 0x00) (to_char Hang);
  Alcotest.(check int) "Hang size" 1 (byte_size Hang);
  Alcotest.(check char) "AddN opcode" (Char.chr 0x01) (to_char (AddN 0));
  Alcotest.(check int) "AddN size" 2 (byte_size (AddN 0));
  Alcotest.(check char) "MoveN opcode" (Char.chr 0x02) (to_char (MoveN 0));
  Alcotest.(check int) "MoveN size" 2 (byte_size (MoveN 0));
  Alcotest.(check char) "Jz opcode" (Char.chr 0x03) (to_char (Jz 0));
  Alcotest.(check int) "Jz size" 5 (byte_size (Jz 0));
  Alcotest.(check char) "Jnz opcode" (Char.chr 0x04) (to_char (Jnz 0));
  Alcotest.(check int) "Jnz size" 5 (byte_size (Jnz 0));
  Alcotest.(check char) "In opcode" (Char.chr 0x05) (to_char In);
  Alcotest.(check int) "In size" 1 (byte_size In);
  Alcotest.(check char) "Out opcode" (Char.chr 0x06) (to_char Out);
  Alcotest.(check int) "Out size" 1 (byte_size Out);
  Alcotest.(check char) "Call opcode" (Char.chr 0x07) (to_char Call);
  Alcotest.(check int) "Call size" 1 (byte_size Call);
  Alcotest.(check char) "Clear opcode" (Char.chr 0x08) (to_char Clear);
  Alcotest.(check int) "Clear size" 1 (byte_size Clear);
  Alcotest.(check char) "TransferN opcode" (Char.chr 0x09) (to_char (TransferN 0));
  Alcotest.(check int) "TransferN size" 2 (byte_size (TransferN 0));
  Alcotest.(check char) "ScanN opcode" (Char.chr 0x0A) (to_char (ScanN 0));
  Alcotest.(check int) "ScanN size" 2 (byte_size (ScanN 0));
  Alcotest.(check char) "AddAt opcode" (Char.chr 0x0B) (to_char (AddAt (0, 0)));
  Alcotest.(check int) "AddAt size" 3 (byte_size (AddAt (0, 0)));
  Alcotest.(check char) "MulTransfer opcode" (Char.chr 0x0C) (to_char (MulTransfer []));
  Alcotest.(check int) "MulTransfer size empty" 2 (byte_size (MulTransfer []));
  Alcotest.(check int) "MulTransfer size 2" 6 (byte_size (MulTransfer [ 1, 2; 3, 4 ]));
  Alcotest.(check char) "ClearMoveN opcode" (Char.chr 0x0D) (to_char (ClearMoveN 0));
  Alcotest.(check int) "ClearMoveN size" 2 (byte_size (ClearMoveN 0));
  Alcotest.(check char) "ClearN opcode" (Char.chr 0x0E) (to_char (ClearN 0));
  Alcotest.(check int) "ClearN size" 2 (byte_size (ClearN 0))
;;

let test_encode_i8_ranges () =
  (* AddN/MoveN/TransferN/ScanN i8 boundary checks *)
  expect_ok_bytes "AddN 127" (AddN 127) [ 0x01; 0x7F ];
  expect_ok_bytes "AddN -128" (AddN (-128)) [ 0x01; 0x80 ];
  expect_error "AddN 128" (AddN 128);
  expect_error "AddN -129" (AddN (-129));
  expect_ok_bytes "MoveN 127" (MoveN 127) [ 0x02; 0x7F ];
  expect_ok_bytes "MoveN -128" (MoveN (-128)) [ 0x02; 0x80 ];
  expect_error "MoveN 128" (MoveN 128);
  expect_error "MoveN -129" (MoveN (-129));
  expect_ok_bytes "TransferN 5" (TransferN 5) [ 0x09; 0x05 ];
  expect_error "TransferN 128" (TransferN 128);
  expect_ok_bytes "ScanN -1" (ScanN (-1)) [ 0x0A; 0xFF ];
  expect_ok_bytes "ClearMoveN -3" (ClearMoveN (-3)) [ 0x0D; 0xFD ];
  expect_ok_bytes "ClearN 2" (ClearN 2) [ 0x0E; 0x02 ]
;;

let test_encode_addat_ranges () =
  expect_ok_bytes "AddAt (0,0)" (AddAt (0, 0)) [ 0x0B; 0x00; 0x00 ];
  expect_ok_bytes "AddAt (-128,127)" (AddAt (-128, 127)) [ 0x0B; 0x80; 0x7F ];
  expect_error "AddAt (128,0)" (AddAt (128, 0));
  expect_error "AddAt (0,128)" (AddAt (0, 128))
;;

let test_encode_jumps_32bit () =
  expect_ok_bytes "Jz 0" (Jz 0) (0x03 :: int32_bytes_le 0);
  expect_ok_bytes "Jz -1" (Jz (-1)) (0x03 :: int32_bytes_le (-1));
  expect_ok_bytes "Jnz 42" (Jnz 42) (0x04 :: int32_bytes_le 42)
;;

let test_encode_multransfer () =
  (* empty pairs *)
  expect_ok_bytes "MulTransfer []" (MulTransfer []) [ 0x0C; 0x00 ];
  (* two pairs *)
  expect_ok_bytes
    "MulTransfer 2"
    (MulTransfer [ 1, 2; -3, -4 ])
    [ 0x0C; 0x02; 0x01; 0x02; 0xFD; 0xFC ];
  (* count bounds *)
  let pairs_128 = List.init 128 (fun _ -> 0, 0) in
  (match encode (MulTransfer pairs_128) with
   | Ok _ -> Alcotest.fail "MulTransfer with 128 pairs should error"
   | Error _ -> ());
  (* operand bounds error mapping *)
  match encode (MulTransfer [ 200, 0 ]) with
  | Ok _ -> Alcotest.fail "MulTransfer bad delta should error"
  | Error _ -> ()
;;

let test_encode_list_and_combine () =
  match encode_list [ AddN 1; MoveN (-2); In; Jz 5 ] with
  | Error _ -> Alcotest.fail "encode_list should succeed"
  | Ok list_bytes ->
    let code = combine_encoded_list list_bytes in
    Alcotest.(check (list int))
      "combined"
      [ 0x01; 0x01; 0x02; 0xFE; 0x05; 0x03; 0x05; 0x00; 0x00; 0x00 ]
      (bytes_to_list code)
;;

let () =
  let open Alcotest in
  run
    "ISA tests"
    [ "meta", [ test_case "to_char and byte_size" `Quick test_to_char_and_size ]
    ; ( "encode"
      , [ test_case "i8 ranges" `Quick test_encode_i8_ranges
        ; test_case "addat ranges" `Quick test_encode_addat_ranges
        ; test_case "jumps 32bit" `Quick test_encode_jumps_32bit
        ; test_case "multransfer" `Quick test_encode_multransfer
        ; test_case "encode_list/combine" `Quick test_encode_list_and_combine
        ] )
    ]
;;
