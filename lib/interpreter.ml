open Ast
module Environment = Map.Make (String)

exception RuntimeError of string

let rec interpret_expr env = function
  | Literal (Identifier id) -> (
      match Environment.find_opt id env with
      | Some l -> l
      | None -> raise (RuntimeError ("Cannot find variable '" ^ id ^ "'")))
  | Literal l -> l
  | Unary { op; expr } -> interpret_unop env op expr
  | Binary { left; op; right } -> interpret_binop env op left right
  | Grouping e -> interpret_expr env e

and interpret_unop env op expr =
  match (op, interpret_expr env expr) with
  | Not, Bool b -> Bool (not b)
  | Neg, Number n -> Number (Float.neg n)
  | Not, _ -> raise (RuntimeError "'!' must be followed by bool")
  | Neg, _ -> raise (RuntimeError "Unary '-' must be followed by a number")

and interpret_binop env op left right =
  match (op, interpret_expr env left, interpret_expr env right) with
  | Less, Number left, Number right -> Bool (left < right)
  | Less_equal, Number left, Number right -> Bool (left <= right)
  | Greater, Number left, Number right -> Bool (left > right)
  | Greater_equal, Number left, Number right -> Bool (left >= right)
  | Equal_equal, left, right -> interpret_equal left right
  | Plus, Number left, Number right -> Number (left +. right)
  | Minus, Number left, Number right -> Number (left -. right)
  | Star, Number left, Number right -> Number (left *. right)
  | Slash, Number left, Number right -> Number (left /. right)
  | Plus, String left, String right -> String (left ^ right)
  | _, _, _ ->
      raise
        (RuntimeError
           ("Cannot use binary op " ^ show_binop op ^ " with expr "
           ^ show_literal (interpret_expr env left)
           ^ " and "
           ^ show_literal (interpret_expr env right)))

and interpret_equal left right =
  match (left, right) with
  | Bool l, Bool r -> Bool (Bool.equal l r)
  | String l, String r -> Bool (String.equal l r)
  | Number l, Number r -> Bool (Float.equal l r)
  | Nil, Nil -> Bool true
  | _ ->
      raise
        (RuntimeError
           ("Cannot check equality betwenn " ^ show_literal left ^ " and "
          ^ show_literal right))

let interpret_stmt env = function
  | Expr e ->
      ignore (interpret_expr env e);
      env
  | Print e ->
      print_endline (interpret_expr env e |> show_literal);
      env

let interpret_decl env = function
  | Var_decl (id, expr) -> Environment.add id (interpret_expr env expr) env
  | Stmt s -> interpret_stmt env s

let interpret =
  List.fold_left (fun env decl -> interpret_decl env decl) Environment.empty
