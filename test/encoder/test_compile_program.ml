open Encoder.Compiler

let bytes_to_list b =
  let len = Bytes.length b in
  let rec loop acc i =
    if i = len then List.rev acc else loop (Bytes.get_uint8 b i :: acc) (i + 1)
  in
  loop [] 0
;;

let show_error = function
  | UnmatchedClosingBracket _ -> "UnmatchedClosingBracket"
  | UnmatchedOpeningBracket _ -> "UnmatchedOpeningBracket"
  | EncodingError _ -> "EncodingError"
;;

let helloworld_source =
  "+++++++++++[>++++++>+++++++++>++++++++>++++>+++>+<<<<<<-]>+++\n"
  ^ "+++.>++.+++++++..+++.>>.>-.<<-.<.+++.------.--------.>>>+.>-."
;;

let test_compile_helloworld () =
  match compile helloworld_source with
  | Ok b ->
    let actual = bytes_to_list b in
    let expected =
      [ 0x01
      ; 0x0B
      ; 0x14
      ; 0x06
      ; 0x01
      ; 0x06
      ; 0x02
      ; 0x09
      ; 0x03
      ; 0x08
      ; 0x04
      ; 0x04
      ; 0x05
      ; 0x03
      ; 0x06
      ; 0x01
      ; 0x0C
      ; 0x01
      ; 0x06
      ; 0x06
      ; 0x0C
      ; 0x01
      ; 0x02
      ; 0x06
      ; 0x01
      ; 0x07
      ; 0x06
      ; 0x06
      ; 0x01
      ; 0x03
      ; 0x06
      ; 0x02
      ; 0x02
      ; 0x06
      ; 0x0C
      ; 0x0B
      ; 0x06
      ; 0x02
      ; 0xFE
      ; 0x0B
      ; 0x06
      ; 0x0D
      ; 0x06
      ; 0x01
      ; 0x03
      ; 0x06
      ; 0x01
      ; 0xFA
      ; 0x06
      ; 0x01
      ; 0xF8
      ; 0x06
      ; 0x02
      ; 0x03
      ; 0x0A
      ; 0x06
      ; 0x0C
      ; 0x0B
      ; 0x06
      ]
    in
    Alcotest.(check (list int)) "helloworld bytecode" expected actual
  | Error err -> Alcotest.failf "compile error: %s" (show_error err)
;;

let () =
  let open Alcotest in
  run
    "Compile program tests"
    [ "compile", [ test_case "helloworld" `Quick test_compile_helloworld ] ]
;;
