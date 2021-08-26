module Ast = Ast
open Lexing

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
      try
        ignore (Static_analysis.make decls);
        try ignore (Interpreter.interpret decls)
        with Interpreter.RuntimeError err ->
          prerr_string "RuntimeError: ";
          prerr_endline err
      with Static_analysis.StaticError err ->
        prerr_string "StaticError: ";
        prerr_endline err)
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
