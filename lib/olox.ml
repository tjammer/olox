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

let rec collect lexbuf lst = function
  | Parser.Eof -> List.rev lst
  | other -> collect lexbuf (other :: lst) (Lexer.read lexbuf)

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf
  with Lexer.SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    failwith "Could not lex"

let run src =
  let lexbuf = Lexing.from_string src in
  parse_with_error lexbuf |> Ast.string_of_expr |> print_endline;
  Ok ()

let run_file filename =
  let content = CCIO.(with_in filename read_all) in
  match run content with
  | Ok _ -> ()
  | Error (line, message) -> report line "" message

let rec run_prompt () =
  try
    print_string "> ";
    ignore @@ run (read_line ());
    run_prompt ()
  with End_of_file -> ()
