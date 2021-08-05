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

type expr =
  | Literal of literal
  | Unary of { op : unop; expr : expr }
  | Binary of { left : expr; op : binop; right : expr }
  | Grouping of expr

let parenthesize str = "(" ^ String.concat " " str ^ ")"

let rec show_expr = function
  | Literal lit -> show_literal lit
  | Unary { op; expr } -> parenthesize [ show_unop op; show_expr expr ]
  | Binary { left; op; right } ->
      parenthesize [ show_binop op; show_expr left; show_expr right ]
  | Grouping expr -> parenthesize [ "group"; show_expr expr ]
