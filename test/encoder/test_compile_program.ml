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
      ; 3
      ; 25
      ; 0
      ; 0
      ; 0
      ; 12
      ; 1
      ; 6
      ; 12
      ; 1
      ; 9
      ; 12
      ; 1
      ; 8
      ; 12
      ; 1
      ; 4
      ; 12
      ; 1
      ; 3
      ; 12
      ; 10
      ; 2
      ; 250
      ; 11
      ; 4
      ; 231
      ; 255
      ; 255
      ; 255
      ; 12
      ; 1
      ; 3
      ; 1
      ; 3
      ; 6
      ; 12
      ; 1
      ; 2
      ; 6
      ; 1
      ; 7
      ; 6
      ; 6
      ; 1
      ; 3
      ; 6
      ; 2
      ; 2
      ; 6
      ; 12
      ; 11
      ; 6
      ; 2
      ; 254
      ; 11
      ; 6
      ; 13
      ; 6
      ; 1
      ; 3
      ; 6
      ; 1
      ; 250
      ; 6
      ; 1
      ; 248
      ; 6
      ; 2
      ; 3
      ; 10
      ; 6
      ; 12
      ; 11
      ; 6
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
