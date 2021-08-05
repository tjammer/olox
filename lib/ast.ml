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
  | Plus
  | Minus
  | Star
  | Slash
  | Equal_equal
[@@deriving show]

type expr =
  | Literal of literal
  | Unary of { op : unop; expr : expr }
  | Binary of { left : expr; op : binop; right : expr }
  | Grouping of expr

let parenthesize str = "(" ^ String.concat " " str ^ ")"

let rec string_of_expr = function
  | Literal lit -> show_literal lit
  | Unary { op; expr } -> parenthesize [ show_unop op; string_of_expr expr ]
  | Binary { left; op; right } ->
      parenthesize [ show_binop op; string_of_expr left; string_of_expr right ]
  | Grouping expr -> parenthesize [ "group"; string_of_expr expr ]
