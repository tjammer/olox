{
open Parser
open Lexing

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | float    { Number (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"   { True }
  | "false"  { False }
  | "null"   { Nil }
  | "and"    { And }
  | "print"  { Print }
  | "var"    { Var }
  | "if"     { If }
  | "else"   { Else }
  | "and"    { And }
  | "or"     { Or }
  | "while"  { While }
  | "for"    { For }
  | "fun"    { Fun }
  | id       { Identifier (Lexing.lexeme lexbuf) }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | '('      { Left_paren }
  | ')'      { Right_paren }
  | '{'      { Left_brace }
  | '}'      { Right_brace }
  | ','      { Comma }
  | '.'      { Dot }
  | '-'      { Minus }
  | '+'      { Plus }
  | ';'      { Semicolon }
  | '*'      { Star }
  | '!'      { Bang }
  | '='      { Equal }
  | '/'      { Slash }
  | "!="     { Bang_equal }
  | "=="     { Equal_equal }
  | '<'      { Less }
  | '>'      { Greater }
  | "<="     { Less_equal }
  | ">="     { Greater_equal }
  | "//"     { line_comment lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { Eof }

and read_string buf =
  parse
  | '"'       { String (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and line_comment =
  parse
  | newline { next_line lexbuf; read lexbuf }
  | eof     { Eof }
  | _       { line_comment lexbuf }
