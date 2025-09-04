open Core
open Stdio
open Cmdliner
open Cmdliner.Term.Syntax

type optimized_error =
  | CompileError of Compiler.error
  | VMError of Runtime.Interpreter.error

let pp_optimized_error fmt = function
  | CompileError err -> Format.fprintf fmt "compile error: %a" Compiler.pp_error err
  | VMError err -> Format.fprintf fmt "VM error: %a" Runtime.Interpreter.pp_error err
;;

type optimization =
  | Fuse
  | Patterns

type optimize_flags =
  { fuse : bool
  ; patterns : bool
  }

let all_optimizations = { fuse = true; patterns = true }

let apply_disable (flags : optimize_flags) = function
  | Fuse -> { flags with fuse = false }
  | Patterns -> { flags with patterns = false }
;;

let apply_enable (flags : optimize_flags) = function
  | Fuse -> { flags with fuse = true }
  | Patterns -> { flags with patterns = true }
;;

let build_ir (flags : optimize_flags) (source : string) =
  let open Compiler in
  let ir0 = parse_sequence source in
  let ir1 = if flags.fuse then fuse_std_ops ir0 else ir0 in
  let ir2 = if flags.patterns then Pattern_optimizer.run ir1 else ir1 in
  let ir3 = if flags.fuse then fuse_std_ops ir2 else ir2 in
  ir3
;;

let pp_ir ~spacing ir =
  let open Isa in
  Format.printf "%a\n" (pp_intr_indent ~spacing) ir
;;

let compile (flags : optimize_flags) (source : string)
  : (Isa.t list, optimized_error) result
  =
  source
  |> build_ir flags
  |> Compiler.resolve_jumps
  |> Result.map_error ~f:(fun err -> CompileError err)
;;

let run_vm (program : Isa.t list) : (unit, optimized_error) result =
  let open Runtime.Interpreter in
  let vm = create ~memory:(Tape.create ~max_size:(1 lsl 26) 16000) program in
  run vm |> Result.map_error ~f:(fun err -> VMError err)
;;

let run_raw program =
  match Runtime.Basic.run program with
  | Error err -> Format.eprintf "error: %a@\n" Runtime.Basic.pp_error err
  | Ok _ -> ()
;;

let optimization_of_string = function
  | "fuse" -> Ok Fuse
  | "patterns" -> Ok Patterns
  | s ->
    Error (`Msg (Printf.sprintf "unknown optimization '%s' (expected: fuse|patterns)" s))
;;

let optimization_list_conv : optimization list Arg.conv =
  let parse s =
    let parts =
      String.split ~on:',' s |> List.filter ~f:(fun x -> not (String.is_empty x))
    in
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | hd :: tl ->
        (match optimization_of_string (String.strip hd) with
         | Ok o -> loop (o :: acc) tl
         | Error _ as e -> e)
    in
    loop [] parts
  in
  let print ppf lst =
    let one = function
      | Fuse -> "fuse"
      | Patterns -> "patterns"
    in
    Format.fprintf ppf "%s" (String.concat ~sep:"," (List.map lst ~f:one))
  in
  Arg.conv (parse, print)
;;

let raw_flag =
  Arg.(
    value
    & flag
    & info
        [ "raw" ]
        ~doc:"Run the raw interpreter; ignore optimization and timing options")
;;

let disable_opt =
  let doc =
    "Disable optimizations. Accepts a comma-separated list; can be provided multiple \
     times. FLAGS: $(b,fuse), $(b,patterns)."
  in
  Arg.(
    value
    & opt_all optimization_list_conv []
    & info [ "disable-optimize"; "D" ] ~doc ~docv:"FLAGS")
;;

let enable_opt =
  let doc =
    "Enable optimizations. Accepts a comma-separated list; can be provided multiple \
     times. FLAGS: $(b,fuse), $(b,patterns)."
  in
  Arg.(
    value
    & opt_all optimization_list_conv []
    & info [ "enable-optimize"; "O" ] ~doc ~docv:"FLAGS")
;;

let pp_compiled_flag =
  Arg.(
    value
    & flag
    & info [ "pp-compiled" ] ~doc:"Pretty-print the compiled IR (after optimizations)")
;;

let time_flag =
  Arg.(
    value & flag & info [ "time" ] ~doc:"Show compilation and (if applicable) run timings")
;;

let instructions_flags =
  Arg.(value & flag & info [ "instructions" ] ~doc:"Show compiled instruction count")
;;

let dry_flag =
  Arg.(value & flag & info [ "dry" ] ~doc:"Compile only; do not run the compiled program")
;;

let infile =
  let doc = "$(docv) is the input file. Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"FILE")
;;

let tool ~raw ~disable ~enable ~pp_compiled ~time ~instructions ~dry ~infile =
  let program =
    if String.(infile = "-")
    then In_channel.input_all stdin
    else In_channel.read_all infile
  in
  if raw
  then (
    run_raw program;
    Cmdliner.Cmd.Exit.ok)
  else (
    let flags =
      let disable = List.concat disable in
      let enable = List.concat enable in
      let after_disable =
        List.fold_left ~f:apply_disable ~init:all_optimizations disable
      in
      let after_enable = List.fold_left ~f:apply_enable ~init:after_disable enable in
      after_enable
    in
    let compile_start = if time then Some (Time_ns.now ()) else None in
    match compile flags program with
    | Error err ->
      Format.eprintf "error: %a@\n" pp_optimized_error err;
      1
    | Ok compiled_program ->
      (match instructions with
       | true ->
         Out_channel.printf
           "Compiled %d raw instructions to %d\n\n"
           (String.length program)
           (List.length compiled_program)
       | false -> ());
      (match pp_compiled with
       | true -> build_ir flags program |> pp_ir ~spacing:2
       | false -> ());
      (match compile_start, time with
       | Some t0, true ->
         let dt = Time_ns.diff (Time_ns.now ()) t0 in
         Out_channel.printf "Compile time: %s\n" (Time_ns.Span.to_string dt)
       | _ -> ());
      if dry
      then Cmdliner.Cmd.Exit.ok
      else (
        let run_start = if time then Some (Time_ns.now ()) else None in
        match run_vm compiled_program with
        | Ok () ->
          (match run_start, time with
           | Some t0, true ->
             let dt = Time_ns.diff (Time_ns.now ()) t0 in
             Out_channel.printf "Run time: %s\n" (Time_ns.Span.to_string dt)
           | _ -> ());
          Cmdliner.Cmd.Exit.ok
        | Error err ->
          Format.eprintf "error: %a@\n" pp_optimized_error err;
          1))
;;

let cmd =
  let doc = "Brainfuck interpreter/VM with optional optimizations" in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P "By default runs the optimized VM with all optimizations enabled."
    ; `P
        "Use $(b,--raw) to run the raw interpreter; optimization flags, pretty-printing, \
         timing and dry-run are ignored in that mode."
    ]
  in
  Cmd.v (Cmd.info "camelfuck" ~doc ~man)
  @@
  let+ raw = raw_flag
  and+ disable = disable_opt
  and+ enable = enable_opt
  and+ pp_compiled = pp_compiled_flag
  and+ time = time_flag
  and+ instructions = instructions_flags
  and+ dry = dry_flag
  and+ infile = infile in
  tool ~raw ~disable ~enable ~pp_compiled ~time ~instructions ~dry ~infile
;;

let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())
