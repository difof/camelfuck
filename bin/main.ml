open Core
open Stdio

module Brainfuck = struct
  type jump_table = int Int.Table.t
  type memory = Tape.t

  type t =
    { program : string
    ; program_length : int
    ; memory : memory
    ; jump_table : jump_table
    ; output : Out_channel.t
    ; input : In_channel.t
    ; mutable pc : int
    }

  type error =
    | MissingOpenBracket of int
    | MissingCloseBracket of int
    | TapeError of Tape.error

  let pp_error fmt = function
    | MissingOpenBracket pc -> Format.fprintf fmt "missing open bracket at %d" pc
    | MissingCloseBracket pc -> Format.fprintf fmt "missing closing bracket for %d" pc
    | TapeError err -> Tape.pp_error fmt err
  ;;

  let make_jump_table p =
    let jump_table = Hashtbl.create (module Int) in
    let result =
      String.foldi p ~init:(Ok []) ~f:(fun pc acc ch ->
        match acc with
        | Error _ as err -> err
        | Ok stack ->
          (match ch with
           | '[' -> Ok (pc :: stack)
           | ']' ->
             (match stack with
              | j :: rest ->
                Hashtbl.set jump_table ~key:pc ~data:j;
                Hashtbl.set jump_table ~key:j ~data:pc;
                Ok rest
              | [] -> Error (MissingOpenBracket (pc + 1)))
           | _ -> Ok stack))
    in
    match result with
    | Ok [] -> Ok jump_table
    | Ok (hd :: _) -> Error (MissingCloseBracket (hd + 1))
    | Error _ as err -> err
  ;;

  let of_string (input, output) p =
    make_jump_table p
    |> Result.map ~f:(fun jump_table ->
      { program = p
      ; program_length = String.length p
      ; pc = 0
      ; memory = Tape.create 4096
      ; jump_table
      ; input
      ; output
      })
  ;;

  let advance t =
    t.pc <- t.pc + 1;
    t
  ;;

  let get_cell t = Tape.get t.memory

  let cell_op t f =
    Tape.set t.memory @@ f @@ get_cell t;
    t
  ;;

  let jump_matching t =
    (* it will never throw since jump table is already validated *)
    t.pc <- Hashtbl.find_exn t.jump_table t.pc;
    t
  ;;

  let exec_instr t =
    match t.program.[t.pc] with
    | '>' ->
      Tape.move_exn t.memory 1;
      t
    | '<' ->
      Tape.move_exn t.memory (-1);
      t
    | '+' -> cell_op t (fun v -> v + 1)
    | '-' -> cell_op t (fun v -> v - 1)
    | '.' ->
      get_cell t |> Char.of_int_exn |> Out_channel.output_char t.output;
      (* flush per . to see output immediately *)
      Out_channel.flush t.output;
      t
    | ',' ->
      cell_op t (fun _ ->
        In_channel.input_char t.input |> Option.value ~default:'\000' |> Char.to_int)
    | '[' -> if get_cell t = 0 then jump_matching t else t
    | ']' -> if get_cell t <> 0 then jump_matching t else t
    | _ -> t
  ;;

  let rec eval_exn t =
    if t.pc >= t.program_length then t else eval_exn (exec_instr t |> advance)
  ;;

  let run p =
    try p |> of_string (stdin, stdout) |> Result.map ~f:eval_exn with
    | Tape.TapeExn err -> Error (TapeError err)
  ;;
end

type optimized_error =
  | CompileError of Compiler.error
  | VMError of Runtime.Isa_vm.error

let pp_optimized_error fmt = function
  | CompileError err -> Format.fprintf fmt "compile error: %a" Compiler.pp_error err
  | VMError err -> Format.fprintf fmt "VM error: %a" Runtime.Isa_vm.pp_error err
;;

let run_optimized source =
  let open Result in
  let open Compiler in
  let open Runtime.Isa_vm in
  let start = Time_ns.now () in
  source
  |> full_pass
  |> map_error ~f:(fun err -> CompileError err)
  >>= fun program ->
  let vm = create ~memory:(Tape.create ~max_size:(1 lsl 26) 16000) program in
  let end_prep = Time_ns.diff (Time_ns.now ()) start in
  let start = Time_ns.now () in
  let result = vm |> run |> map_error ~f:(fun err -> VMError err) in
  let end_run = Time_ns.diff (Time_ns.now ()) start in
  Out_channel.printf
    "Program size %d instructions -> compiled %d instructions\n"
    (String.length source)
    (List.length program);
  Out_channel.printf
    "Took %s to preload and %s to run\n"
    (Time_ns.Span.to_string end_prep)
    (Time_ns.Span.to_string end_run);
  result
;;

let run_raw program =
  match Brainfuck.run program with
  | Error err -> Format.eprintf "error: %a@\n" Brainfuck.pp_error err
  | Ok _ -> ()
;;

type mode =
  | Raw
  | Optimized

let parse_mode_and_program () =
  let mode_ref = ref Optimized in
  let filename_ref = ref None in
  let set_mode = function
    | "raw" -> mode_ref := Raw
    | "optimized" -> mode_ref := Optimized
    | s -> failwith ("unknown mode: " ^ s)
  in
  let speclist : (string * Stdlib.Arg.spec * string) list =
    [ "--raw", Stdlib.Arg.Unit (fun () -> mode_ref := Raw), "Run interpreter (raw)"
    ; ( "--optimized"
      , Stdlib.Arg.Unit (fun () -> mode_ref := Optimized)
      , "Run optimized VM (default)" )
    ; ( "-m"
      , Stdlib.Arg.Symbol ([ "raw"; "optimized" ], set_mode)
      , "MODE Select run mode: raw|optimized" )
    ; ( "--mode"
      , Stdlib.Arg.Symbol ([ "raw"; "optimized" ], set_mode)
      , "MODE Select run mode: raw|optimized" )
    ]
  in
  let anon_fun s = filename_ref := Some s in
  Stdlib.Arg.parse speclist anon_fun "usage: camelfuck [--raw|--optimized|-m MODE] [file]";
  let program =
    match !filename_ref with
    | Some filename ->
      (try In_channel.read_all filename with
       | _ -> In_channel.input_all stdin)
    | None -> In_channel.input_all stdin
  in
  !mode_ref, program
;;

let () =
  let mode, program = parse_mode_and_program () in
  match mode with
  | Raw -> run_raw program
  | Optimized ->
    (match run_optimized program with
     | Ok _ -> ()
     | Error err ->
       Format.eprintf "error: %a@\n" pp_optimized_error err;
       exit 1)
;;

(*
   benchmarks

time dune exec camelfuck -- --raw bfprograms/mandlebrot.bf        = 56.73s user 0.51s system 98% cpu 58.269 total
time dune exec camelfuck -- --optimized bfprograms/mandlebrot.bf  = 17.47s user 0.24s system 96% cpu 18.414 total
*)
