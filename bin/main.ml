open Core
open Stdio

(* Unbounded bi-directional tape memory *)
module Tape : sig
  type t

  val create : unit -> t
  val get_current : t -> int option
  val set_current : t -> int -> unit
  val forward : t -> unit
  val backward : t -> unit
end = struct
  type t =
    { mutable pos : int
    ; cells : int Int.Table.t
    }

  let create () = { pos = 0; cells = Hashtbl.create (module Int) }
  let get_current t = Hashtbl.find t.cells t.pos

  let set_current t v =
    let v = v land 0xFF in
    Hashtbl.set t.cells ~key:t.pos ~data:v
  ;;

  let forward t = t.pos <- t.pos + 1
  let backward t = t.pos <- t.pos - 1
end

module Brainfuck : sig
  type t
  type error

  val of_string : In_channel.t * Out_channel.t -> string -> (t, error) result
  val eval : t -> (t, error) result
  val pp_error : Format.formatter -> error -> unit
end = struct
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
        | Error _ as e -> e
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
    | Error _ as e -> e
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

  let get_cell t = Tape.get_current t.memory |> Option.value ~default:0

  let cell_op t f =
    Tape.set_current t.memory @@ f @@ get_cell t;
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
      Tape.forward t.memory;
      t
    | '<' ->
      Tape.backward t.memory;
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

let run p = Brainfuck.(p |> of_string (stdin, stdout) |> Result.bind ~f:eval)

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
  | Error err -> Format.eprintf "error: %a@\n" Brainfuck.pp_error err
  | Ok _ -> ()
;;
