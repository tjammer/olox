open Ast

exception RuntimeError of string

module Environment = struct
  module StrMap = Map.Make (String)

  type t = literal StrMap.t list

  let empty : t = [ StrMap.empty ]

  let add ~id value = function
    | [] ->
        (* We print a warning, b/c this should never happen.
         * It's not actually a problem though *)
        prerr_endline "Internal error: There is an empty environment. How?";
        [ StrMap.(add id value StrMap.empty) ]
    | env :: envs -> StrMap.(add id value env) :: envs

  let replace ~id value env =
    let rec aux ~id value head = function
      | [] -> raise (RuntimeError ("Variable " ^ id ^ " does not exist"))
      | env :: envs -> (
          match StrMap.find_opt id env with
          | Some _ ->
              let env = StrMap.(add id value env) in
              List.rev head @ (env :: envs)
          | None -> aux ~id value (env :: head) envs)
    in
    aux ~id value [] env

  let rec find ~id = function
    | [] -> None
    | env :: envs -> (
        match StrMap.find_opt id env with
        | Some value -> Some value
        | None -> find ~id envs)

  let open_block = List.cons StrMap.empty

  let close_block = function
    | [] ->
        (* As above, this should never happen. *)
        prerr_endline "Internal error: There is an empty environment. How?";
        empty
    | _ :: env -> env
end

let envi = ref Environment.empty

let rec interpret_expr = function
  (* Since olox is in imperative language, an expression can update the env
   * as a side effect. We use a ref for that *)
  | Literal (Identifier id) -> (
      match Environment.find ~id !envi with
      | Some l -> l
      | None -> raise (RuntimeError ("Cannot find variable '" ^ id ^ "'")))
  | Literal l -> l
  | Unary { op; expr } -> interpret_unop op expr
  | Binary { left; op; right } -> interpret_binop op left right
  | Grouping e -> interpret_expr e
  | Assign (id, expr) ->
      let value = interpret_expr expr in
      envi := Environment.replace ~id value !envi;
      value
  | Logical (op, left, right) -> interpret_logicalop op left right

and interpret_logicalop op left right =
  match (interpret_expr left, interpret_expr right) with
  | Bool left, Bool right ->
      Bool (match op with And -> left && right | Or -> left || right)
  | _ ->
      raise (RuntimeError "Expression in if statement must evaluate to a bool")

and interpret_unop op expr =
  match (op, interpret_expr expr) with
  | Not, Bool b -> Bool (not b)
  | Neg, Number n -> Number (Float.neg n)
  | Not, _ -> raise (RuntimeError "'!' must be followed by bool")
  | Neg, _ -> raise (RuntimeError "Unary '-' must be followed by a number")

and interpret_binop op left right =
  match (op, interpret_expr left, interpret_expr right) with
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
           ^ show_literal (interpret_expr left)
           ^ " and "
           ^ show_literal (interpret_expr right)))

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

let rec interpret_stmt = function
  | Expr e -> ignore (interpret_expr e)
  | Print e -> print_endline (interpret_expr e |> show_literal)
  | Block decls ->
      envi := Environment.open_block !envi;
      List.iter interpret_decl decls;
      envi := Environment.close_block !envi
  | If (expr, then', else') -> interpret_if expr then' else'
  | While (expr, stmt) -> interpret_while expr stmt

and interpret_if expr then' else' =
  match interpret_expr expr with
  | Bool bool -> (
      if bool then interpret_stmt then'
      else match else' with Some else' -> interpret_stmt else' | None -> ())
  | _ ->
      raise (RuntimeError "Expression in if condition must evaluate to a bool")

and interpret_while expr stmt =
  let condition () =
    match interpret_expr expr with
    | Bool bool -> bool
    | _ ->
        raise
          (RuntimeError "Expression in while condition must evaluate to a bool")
  in
  while condition () do
    interpret_stmt stmt
  done

and interpret_decl = function
  | Var_decl (id, expr) ->
      let expr =
        match expr with Some expr -> interpret_expr expr | None -> Nil
      in
      envi := Environment.add ~id expr !envi
  | Stmt s -> ignore (interpret_stmt s)

let interpret = List.iter interpret_decl
