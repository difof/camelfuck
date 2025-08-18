open Core
open Stdio

(*module Brainfuck : sig
  type t
  type error

  val of_string : In_channel.t * Out_channel.t -> string -> (t, error) result
  val eval : t -> (t, error) result
  val pp_error : Format.formatter -> error -> unit
end = *)
module Brainfuck (Tape : Tape.S) = struct
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

  let pp_error fmt = function
    | MissingOpenBracket pc -> Format.fprintf fmt "missing open bracket at %d" pc
    | MissingCloseBracket pc -> Format.fprintf fmt "missing closing bracket for %d" pc
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
      ; memory = Tape.create ()
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
      Tape.move t.memory 1;
      t
    | '<' ->
      Tape.move t.memory (-1);
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

  let rec eval t =
    if t.pc >= t.program_length then Ok t else eval (exec_instr t |> advance)
  ;;
end

module BrainfuckHashTape = Brainfuck (Tape.HashTape)

let run p = BrainfuckHashTape.(p |> of_string (stdin, stdout) |> Result.bind ~f:eval)

let program_from_argv_or_stdin () =
  let argv = Sys.get_argv () in
  if Array.length argv > 1
  then (
    let filename = argv.(1) in
    try In_channel.read_all filename with
    (* TODO: report exceptions *)
    | _ -> In_channel.input_all stdin)
  else In_channel.input_all stdin
;;

let () =
  match program_from_argv_or_stdin () |> run with
  | Error err -> Format.eprintf "error: %a@\n" BrainfuckHashTape.pp_error err
  | Ok _ -> ()
;;
