open Containers
module StrTable = Hashtbl.Make (String)

module Token = struct
  type kind =
    | Left_paren (* Single-character tokens. *)
    | Right_paren
    | Left_brace
    | Right_brace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    | Bang (* One or two character tokens. *)
    | Bang_equal
    | Equal
    | Equal_equal
    | Greater
    | Greater_equal
    | Less
    | Less_equal
    | Identifier of string (* Literals. *)
    | String of string
    | Number of float
    | And (* Keywords. *)
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While
    | Eof
  [@@deriving show, eq]

  type t = { kind : kind; line : int } [@@deriving show, eq]
end

let keywords =
  let tbl = StrTable.create 10 in
  StrTable.add tbl "and" Token.And;
  StrTable.add tbl "class" Class;
  StrTable.add tbl "else" Else;
  StrTable.add tbl "false" False;
  StrTable.add tbl "for" For;
  StrTable.add tbl "fun" Fun;
  StrTable.add tbl "if" If;
  StrTable.add tbl "nil" Nil;
  StrTable.add tbl "or" Or;
  StrTable.add tbl "print" Print;
  StrTable.add tbl "return" Return;
  StrTable.add tbl "super" Super;
  StrTable.add tbl "this" This;
  StrTable.add tbl "true" True;
  StrTable.add tbl "var" Var;
  StrTable.add tbl "while" While;
  tbl

let tokenize src =
  let at_end curr = curr = String.length src in

  let is_digit = function '0' .. '9' -> true | _ -> false in

  let is_digit_index current =
    if at_end current then false else is_digit src.[current]
  in

  let is_alpha = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in

  let is_alpha_numeric c = is_alpha c || is_digit c in

  let get_option i = if at_end i then None else Some src.[i] in

  let rec end_of_line i =
    match get_option i with None | Some '\n' -> i | _ -> end_of_line (i + 1)
  in

  let buf = Buffer.create 4 in

  let rec aux i line tokens =
    if at_end i then { Token.kind = Eof; line } :: tokens |> List.rev
    else
      let tok kind = aux (i + 1) line ({ kind; line } :: tokens) in
      match src.[i] with
      | '(' -> tok Left_paren
      | ')' -> tok Right_paren
      | '{' -> tok Left_brace
      | '}' -> tok Right_brace
      | ',' -> tok Comma
      | '.' -> tok Dot
      | '-' -> tok Minus
      | '+' -> tok Plus
      | ';' -> tok Semicolon
      | '*' -> tok Star
      | '!' | '=' | '<' | '>' | '/' -> multiple i line tokens
      | ' ' | '\r' | '\t' -> aux (i + 1) line tokens
      | '\n' -> aux (i + 1) (line + 1) tokens
      | '"' -> string i line tokens
      | '0' .. '9' -> number i line tokens
      | c when is_alpha c -> identifier i line tokens
      | _ -> failwith "TODO fail correctly"
  (* 1 char lookahead *)
  and multiple i line tokens =
    let return i kind = aux i line ({ kind; line } :: tokens) in
    match (src.[i], get_option (i + 1)) with
    | '!', Some '=' -> return (i + 2) Bang_equal
    | '!', _ -> return (i + 1) Bang
    | '=', Some '=' -> return (i + 2) Equal_equal
    | '=', _ -> return (i + 1) Equal
    | '<', Some '=' -> return (i + 2) Less_equal
    | '<', _ -> return (i + 1) Less
    | '>', Some '=' -> return (i + 2) Greater_equal
    | '>', _ -> return (i + 1) Greater
    | '/', Some '/' ->
        (* Comment. Discard until newline *)
        aux (end_of_line i) line tokens
    | '/', _ -> return (i + 1) Slash
    | _, _ -> failwith "TODO fail correctly"
  (* strings *)
  and string i line tokens =
    Buffer.clear buf;

    let rec inner i line =
      if at_end i then failwith "TODO fail correctly"
      else
        match src.[i] with
        | '"' ->
            let str = Buffer.contents buf in
            aux (i + 1) line ({ kind = String str; line } :: tokens)
        | '\n' -> inner (i + 1) (line + 1)
        | c ->
            Buffer.add_char buf c;
            inner (i + 1) line
    in
    inner (i + 1) line
  (* numbers *)
  and number i line tokens =
    Buffer.clear buf;

    let rec inner i =
      match get_option i with
      | Some ('0' .. '9' as c) ->
          Buffer.add_char buf c;
          inner (i + 1)
      | Some ('.' as c) when is_digit_index (i + 1) ->
          Buffer.add_char buf c;
          inner (i + 1)
      | None | Some _ ->
          let num = Buffer.contents buf |> float_of_string in
          aux i line ({ kind = Number num; line } :: tokens)
    in
    inner i
  (* identifier *)
  and identifier i line tokens =
    Buffer.clear buf;

    let rec inner i =
      if at_end i || not (is_alpha_numeric src.[i]) then
        let str = Buffer.contents buf in
        let kind =
          match StrTable.find_opt keywords str with
          | Some keyword -> keyword
          | None -> Identifier str
        in
        aux i line ({ kind; line } :: tokens)
      else (
        Buffer.add_char buf src.[i];
        inner (i + 1))
    in
    inner i
  in

  aux 0 1 []
