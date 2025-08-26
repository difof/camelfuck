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
      [ 1
      ; 11
      ; 5
      ; 25
      ; 0
      ; 0
      ; 0
      ; 14
      ; 1
      ; 6
      ; 14
      ; 1
      ; 9
      ; 14
      ; 1
      ; 8
      ; 14
      ; 1
      ; 4
      ; 14
      ; 1
      ; 3
      ; 14
      ; 12
      ; 4
      ; 6
      ; 13
      ; 6
      ; 231
      ; 255
      ; 255
      ; 255
      ; 14
      ; 1
      ; 3
      ; 1
      ; 3
      ; 8
      ; 14
      ; 1
      ; 2
      ; 8
      ; 1
      ; 7
      ; 8
      ; 8
      ; 1
      ; 3
      ; 8
      ; 3
      ; 2
      ; 8
      ; 14
      ; 13
      ; 8
      ; 4
      ; 2
      ; 13
      ; 8
      ; 15
      ; 8
      ; 1
      ; 3
      ; 8
      ; 2
      ; 6
      ; 8
      ; 2
      ; 8
      ; 8
      ; 3
      ; 3
      ; 12
      ; 8
      ; 14
      ; 13
      ; 8
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
