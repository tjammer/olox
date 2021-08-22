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

type value =
  | Number of float
  | String of string
  | Identifier of string
  | Bool of bool
  | Nil
  | Fun of { callable : string; call : value list -> value }
  | Class of string
  | Instance of (string * (value Environment.t ref[@opaque]))
(* We make the env opaque to since we don't really care about the content *)
(* The env is a reference to make it easier to set values *)
(* TODO use with_path = false *)
[@@deriving show]

and primary = Value of value | Grouping of expr

and expr =
  | Primary of primary
  | Unary of { op : unop; expr : expr }
  | Binary of { left : expr; op : binop; right : expr }
  | Assign of expr * expr
  | Logical of logicop * expr * expr
  | Call of primary * expr list
  | Class_get of primary * string

and statement =
  | Expr of expr
  | Print of expr
  | Block of decl list
  | If of expr * statement * statement option
  | While of expr * statement
  | Return of expr option

and decl =
  | Var_decl of string * expr option
  | Stmt of statement
  | Fun_decl of func
  | Class_decl of string * func list

and func = { name : string; parameters : string list; body : statement }

let parenthesize str = "(" ^ String.concat " " str ^ ")"

let rec show_expr = function
  | Primary (Value lit) -> show_value lit
  | Primary (Grouping expr) -> parenthesize [ "group"; show_expr expr ]
  | Unary { op; expr } -> parenthesize [ show_unop op; show_expr expr ]
  | Binary { left; op; right } ->
      parenthesize [ show_binop op; show_expr left; show_expr right ]
  | Assign (id, expr) -> parenthesize [ "assign"; show_expr id; show_expr expr ]
  | Logical (op, left, right) ->
      parenthesize [ show_logicop op; show_expr left; show_expr right ]
  | Call (func, parameters) ->
      parenthesize
        [
          show_expr (Primary func);
          List.map show_expr parameters |> String.concat ", ";
        ]
  | Class_get (primary, field) ->
      parenthesize [ "get"; show_primary primary; field ]
(* | Class_set (path, value) ->
 *     parenthesize [ show_expr (Class_get path); show_expr value ] *)

exception Error

let make_lvalue = function
  | Primary (Value (Identifier id)) -> id
  | Class_get (instance, field) ->
      Printf.printf "%s, %s\n%!" (show_primary instance) field;
      "nil"
  | _ -> raise Error
