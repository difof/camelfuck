open Core
open Stdio

type optimized_error =
  | CompileError of Compiler.error
  | VMError of Runtime.Vm.error

let pp_optimized_error fmt = function
  | CompileError err -> Format.fprintf fmt "compile error: %a" Compiler.pp_error err
  | VMError err -> Format.fprintf fmt "VM error: %a" Runtime.Vm.pp_error err
;;

let print_optimized source =
  let open Compiler in
  let open Isa in
  let instructions =
    source |> parse_sequence |> fuse_std_ops |> Pattern_optimizer.run |> fuse_std_ops
  in
  Format.printf "%a\n" (pp_intr_indent ~spacing:2) instructions
;;

let run_optimized source =
  let open Result in
  let open Compiler in
  let open Runtime.Vm in
  Out_channel.print_endline "## IR:";
  source |> print_optimized;
  Out_channel.print_endline "## End";
  let start = Time_ns.now () in
  source
  |> parse_sequence
  |> fuse_std_ops
  |> Pattern_optimizer.run
  |> resolve_jumps
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
  match Runtime.Basic.run program with
  | Error err -> Format.eprintf "error: %a@\n" Runtime.Basic.pp_error err
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
