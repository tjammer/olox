open Ast

exception TypeError of string

let rec interpret = function
  | Literal l -> l
  | Unary { op; expr } -> interpret_unop op expr
  | Binary { left; op; right } -> interpret_binop op left right
  | Grouping e -> interpret e

and interpret_unop op expr =
  match (op, interpret expr) with
  | Not, Bool b -> Bool (not b)
  | Neg, Number n -> Number (Float.neg n)
  | Not, _ -> raise (TypeError "'!' must be followed by bool")
  | Neg, _ -> raise (TypeError "Unary '-' must be followed by a number")

and interpret_binop op left right =
  match (op, interpret left, interpret right) with
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
        (TypeError
           ("Cannot use binary op " ^ show_binop op ^ " with expr "
           ^ show_literal (interpret left)
           ^ " and "
           ^ show_literal (interpret right)))

and interpret_equal left right =
  match (left, right) with
  | Bool l, Bool r -> Bool (Bool.equal l r)
  | String l, String r -> Bool (String.equal l r)
  | Number l, Number r -> Bool (Float.equal l r)
  | Nil, Nil -> Bool true
  | _ ->
      raise
        (TypeError
           ("Cannot check equality betwenn " ^ show_literal left ^ " and "
          ^ show_literal right))
