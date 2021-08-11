type literal =
  | Number of float
  | String of string
  | Identifier of string
  | Bool of bool
  | Nil
[@@deriving show]

type unop = Not | Neg [@@deriving show]

type binop =
  | Less
  | Less_equal
  | Greater
  | Greater_equal
  | Equal_equal
  | Plus
  | Minus
  | Star
  | Slash
[@@deriving show]

type logicop = And | Or [@@deriving show]

type expr =
  | Literal of literal
  | Unary of { op : unop; expr : expr }
  | Binary of { left : expr; op : binop; right : expr }
  | Grouping of expr
  | Assign of string * expr
  | Logical of logicop * expr * expr

type statement =
  | Expr of expr
  | Print of expr
  | Block of decl list
  | If of expr * statement * statement option
  | While of expr * statement

and decl = Var_decl of string * expr | Stmt of statement

let parenthesize str = "(" ^ String.concat " " str ^ ")"

let rec show_expr = function
  | Literal lit -> show_literal lit
  | Unary { op; expr } -> parenthesize [ show_unop op; show_expr expr ]
  | Binary { left; op; right } ->
      parenthesize [ show_binop op; show_expr left; show_expr right ]
  | Grouping expr -> parenthesize [ "group"; show_expr expr ]
  | Assign (id, expr) -> parenthesize [ "assign"; id; show_expr expr ]
  | Logical (op, left, right) ->
      parenthesize [ show_logicop op; show_expr left; show_expr right ]

exception Error

let make_lvalue = function Literal (Identifier id) -> id | _ -> raise Error
