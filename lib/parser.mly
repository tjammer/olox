%{
    open Ast
%}

%token Left_paren
%token Right_paren
%token Left_brace
%token Right_brace
%token Comma
%token Dot
%token Minus
%token Plus
%token Semicolon
%token Slash
%token Star
%token Bang
%token Bang_equal
%token Equal
%token Equal_equal
%token Greater
%token Greater_equal
%token Less
%token Less_equal
%token <string> Identifier
%token <string> String
%token <float> Number
%token And
%token Class
%token Else
%token False
%token Fun
%token For
%token If
%token Nil
%token Or
%token Print
%token Return
%token Super
%token This
%token True
%token Var
%token While
%token Eof

%nonassoc Equal_equal
%nonassoc Less_equal
%nonassoc Less
%nonassoc Greater
%left Plus
%left Star

%nonassoc Minus
%nonassoc Bang
%nonassoc Number

%start <Ast.decl list> prog

%%

prog: s = list(decl); Eof { s }

decl:
  | Var; id = Identifier; Equal; e = expr; Semicolon { Var_decl (id, e) }
  | s = stmt { Stmt s }

stmt:
  | e = expr; Semicolon { Expr e }
  | Print; e = expr; Semicolon { Print e }

expr:
  | num = Number { Literal (Number num) }
  | str = String { Literal (String str) }
  | True { Literal (Bool true) }
  | False { Literal (Bool false) }
  | Nil { Literal Nil }
  | id =  Identifier { Literal (Identifier id) }
  | Bang; e = expr { Unary { op = Not ; expr = e } }
  | Minus; e = expr { Unary { op = Neg ; expr = e } }
  | left = expr; Less; right = expr { Binary { left ; op = Less ; right } }
  | left = expr; Less_equal; right = expr { Binary { left ; op = Less_equal ; right } }
  | left = expr; Greater; right = expr { Binary { left ; op = Greater ; right } }
  | left = expr; Greater_equal; right = expr { Binary { left ; op = Greater_equal ; right } }
  | left = expr; Plus; right = expr { Binary { left ; op = Plus ; right } }
  | left = expr; Minus; right = expr { Binary { left ; op = Minus ; right } }
  | left = expr; Star; right = expr { Binary { left ; op = Star ; right } }
  | left = expr; Slash; right = expr { Binary { left ; op = Slash ; right } }
  | left = expr; Equal_equal; right = expr { Binary { left ; op = Equal_equal ; right } }
  | Left_paren; e = expr; Right_paren { Grouping e }
;
