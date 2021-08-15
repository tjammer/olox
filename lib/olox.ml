module Ast = Ast
open Lexing

module M = struct
  (* We have to copy-paste this from Parser.mli,
   * b/c ppx_import does not work with the current ppx ecosystem *)
  type token = Parser.token =
    | While
    | Var
    | True
    | This
    | Super
    | String of string
    | Star
    | Slash
    | Semicolon
    | Right_paren
    | Right_brace
    | Return
    | Print
    | Plus
    | Or
    | Number of float
    | Nil
    | Minus
    | Less_equal
    | Less
    | Left_paren
    | Left_brace
    | If
    | Identifier of string
    | Greater_equal
    | Greater
    | Fun
    | For
    | False
    | Equal_equal
    | Equal
    | Eof
    | Else
    | Dot
    | Comma
    | Class
    | Bang_equal
    | Bang
    | And
  [@@deriving show]
end

let report line where message =
  Format.eprintf "[line %i] Error%s: %s" line where message

let print_position _ lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Ok (Parser.prog Lexer.read lexbuf) with
  | Lexer.SyntaxError msg ->
      Error (Printf.sprintf "%a: %s\n" print_position lexbuf msg)
  | Parser.Error ->
      Error (Printf.sprintf "%a: syntax error\n" print_position lexbuf)

let run src =
  let lexbuf = Lexing.from_string src in
  match parse_with_error lexbuf with
  | Ok decls -> (
      (* TODO errors in static ana *)
      let stat =  (Static_analysis.make decls) in
      try ignore (Interpreter.interpret stat decls)
      with Interpreter.RuntimeError err ->
        print_string "RuntimeError: ";
        print_endline err)
  | Error message -> prerr_string message

let run_file filename =
  let content = CCIO.(with_in filename read_all) in
  run content

let rec run_prompt () =
  try
    print_string "> ";
    run (read_line ());
    run_prompt ()
  with End_of_file -> ()
