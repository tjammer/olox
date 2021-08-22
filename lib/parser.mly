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

%right Equal
%left Or
%left And
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

prog: decls = list(decl); Eof { decls }

decl:
  | var = var_decl { var }
  | s = stmt { Stmt s }
  | Fun; func { Fun_decl $2 }
  | Class; Identifier; Left_brace; list(func); Right_brace; { Class_decl ($2, $4) }

var_decl:
  | Var; id = Identifier; Equal; e = expr; Semicolon { Var_decl (id, Some e) }
  | Var; id = Identifier; Semicolon { Var_decl (id, None) }

stmt:
  | e = expr; Semicolon { Expr e }
  | Print; e = expr; Semicolon { Print e }
  | blck = block { blck }
  | If; Left_paren; e = expr; Right_paren; thn = stmt; els = else_opt { If (e, thn, els) }
  | While; Left_paren; e = expr; Right_paren; s = stmt { While (e, s) }
  | for_ = for_loop { for_ }
  | Return; e = option(expr); Semicolon { Return e }

block:
  | Left_brace; decls = list(decl); Right_brace { Block decls }

func:
  | Identifier; Left_paren; separated_list(Comma, Identifier); Right_paren;
    block
    { { name = $1; parameters = $3; body = $5 } }

else_opt:
  | Else; s = stmt { Some s }
  |                { None }

/* We desugar to a while loop */
for_loop:
  | For; Left_paren; init = var_decl; cond = expr; Semicolon; incr = expr; Right_paren; s = stmt
    { Block [init; Stmt (While (cond, Block [ Stmt s; Stmt (Expr incr) ]))]}

expr:
  | prim = primary { Primary prim }
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
  | assign { $1 }
  | left = expr; And; right = expr { Logical (And, left, right)}
  | left = expr; Or; right = expr { Logical (Or, left, right)}
  | call { $1 }
  | class_get { $1 }

assign:
  | expr; Equal; expr { Assign ($1, $3) }
/* We check in the static analysis step that the expr is a correct lvalue */

call:
  | primary; Left_paren; separated_list(Comma, expr); Right_paren
    { Call ($1, $3) }

in_chain:
  | call { $1 }
  | Identifier { $1 }

class_get:
  | primary; Dot; Identifier   { Class_get ($1, $3) }

primary:
  | num = Number { Value (Number num) }
  | str = String { Value (String str) }
  | True { Value (Bool true) }
  | False { Value (Bool false) }
  | Nil { Value Nil }
  | id =  Identifier { Value (Identifier id) }
  | Left_paren; e = expr; Right_paren { Grouping e }
;
