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

%left Equal
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
  | func = function_ { func }

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

function_:
  | Fun; id = Identifier; Left_paren; parameters = separated_list(Comma, Identifier); Right_paren;
    body = block
    { Fun_decl { name = id; parameters; body } }

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
  | lval = expr; Equal; e = expr { Assign (make_lvalue lval, e) }
  | left = expr; And; right = expr { Logical (And, left, right)}
  | left = expr; Or; right = expr { Logical (Or, left, right)}
  | cl = call { cl }

call:
  | prim = primary; Left_paren; params = separated_list(Comma, expr); Right_paren
    { Call (prim, params) }

primary:
  | num = Number { Literal (Number num) }
  | str = String { Literal (String str) }
  | True { Literal (Bool true) }
  | False { Literal (Bool false) }
  | Nil { Literal Nil }
  | id =  Identifier { Literal (Identifier id) }
  | Left_paren; e = expr; Right_paren { Grouping e }
;
