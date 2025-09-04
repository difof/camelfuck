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
  Alcotest.(check char) "Add opcode" (Char.chr 0x01) (to_char (Add 0));
  Alcotest.(check int) "Add size" 2 (byte_size (Add 0));
  Alcotest.(check char) "Move opcode" (Char.chr 0x02) (to_char (Move 0));
  Alcotest.(check int) "Move size" 2 (byte_size (Move 0));
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
  Alcotest.(check char)
    "TransferStride opcode"
    (Char.chr 0x09)
    (to_char (TransferStride 0));
  Alcotest.(check int) "TransferStride size" 2 (byte_size (TransferStride 0));
  Alcotest.(check char) "ScanStride opcode" (Char.chr 0x0A) (to_char (ScanStride 0));
  Alcotest.(check int) "ScanStride size" 2 (byte_size (ScanStride 0));
  Alcotest.(check char) "AddAt opcode" (Char.chr 0x0B) (to_char (AddAt (0, 0)));
  Alcotest.(check int) "AddAt size" 3 (byte_size (AddAt (0, 0)));
  Alcotest.(check char)
    "MultiTransfer opcode"
    (Char.chr 0x0C)
    (to_char (MultiTransfer []));
  Alcotest.(check int) "MultiTransfer size empty" 2 (byte_size (MultiTransfer []));
  Alcotest.(check int) "MultiTransfer size 2" 6 (byte_size (MultiTransfer [ 1, 2; 3, 4 ]));
  Alcotest.(check char) "ClearCells opcode" (Char.chr 0x0E) (to_char (ClearCells 0));
  Alcotest.(check int) "ClearCells size" 2 (byte_size (ClearCells 0))
;;

let test_encode_i8_ranges () =
  (* Add/Move/TransferStride/ScanStride i8 boundary checks *)
  expect_ok_bytes "Add 127" (Add 127) [ 0x01; 0x7F ];
  expect_ok_bytes "Add -128" (Add (-128)) [ 0x01; 0x80 ];
  expect_error "Add 128" (Add 128);
  expect_error "Add -129" (Add (-129));
  expect_ok_bytes "Move 127" (Move 127) [ 0x02; 0x7F ];
  expect_ok_bytes "Move -128" (Move (-128)) [ 0x02; 0x80 ];
  expect_error "Move 128" (Move 128);
  expect_error "Move -129" (Move (-129));
  expect_ok_bytes "TransferStride 5" (TransferStride 5) [ 0x09; 0x05 ];
  expect_error "TransferStride 128" (TransferStride 128);
  expect_ok_bytes "ScanStride -1" (ScanStride (-1)) [ 0x0A; 0xFF ];
  expect_ok_bytes "ClearCells 2" (ClearCells 2) [ 0x0E; 0x02 ];
  expect_ok_bytes "ClearCells -3" (ClearCells (-3)) [ 0x0E; 0xFD ]
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
  expect_ok_bytes "MultiTransfer []" (MultiTransfer []) [ 0x0C; 0x00 ];
  (* two pairs *)
  expect_ok_bytes
    "MultiTransfer 2"
    (MultiTransfer [ 1, 2; -3, -4 ])
    [ 0x0C; 0x02; 0x01; 0x02; 0xFD; 0xFC ];
  (* count bounds *)
  let pairs_128 = List.init 128 (fun _ -> 0, 0) in
  (match encode (MultiTransfer pairs_128) with
   | Ok _ -> Alcotest.fail "MultiTransfer with 128 pairs should error"
   | Error _ -> ());
  (* operand bounds error mapping *)
  match encode (MultiTransfer [ 200, 0 ]) with
  | Ok _ -> Alcotest.fail "MultiTransfer bad delta should error"
  | Error _ -> ()
;;

let test_encode_list_and_combine () =
  match encode_list [ Add 1; Move (-2); In; Jz 5 ] with
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
