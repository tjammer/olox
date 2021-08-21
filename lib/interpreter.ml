open Ast
open Containers

exception RuntimeError of string

let env = ref Globals.globals

let rec interpret_expr = function
  (* Since olox is in imperative language, an expression can update the env
   * as a side effect. We use a ref for that *)
  | Primary primary -> interpret_primary primary
  | Unary { op; expr } -> interpret_unop op expr
  | Binary { left; op; right } -> interpret_binop op left right
  | Assign (name, expr) ->
      let value = interpret_expr expr in
      env := Environment.replace ~name value !env;
      value
  | Logical (op, left, right) -> interpret_logicalop op left right
  | Call (func, arguments) -> interpret_call func arguments

and interpret_primary = function
  | Literal (Identifier name) -> (
      match Environment.find ~name !env with
      | Some l -> l
      | None -> raise (RuntimeError ("Cannot find variable '" ^ name ^ "'")))
  | Literal l -> l
  | Grouping e -> interpret_expr e

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
  | Less, Number left, Number right -> Bool (left <. right)
  | Less_equal, Number left, Number right -> Bool (left <=. right)
  | Greater, Number left, Number right -> Bool (left >. right)
  | Greater_equal, Number left, Number right -> Bool (left >=. right)
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
           ("Cannot check equality between " ^ show_literal left ^ " and "
          ^ show_literal right))

and interpret_call func args =
  match interpret_expr (Primary func) with
  | Fun func ->
      (* Arity check happens in the closure *)
      func.call (List.map interpret_expr args)
  | l ->
      raise
        (RuntimeError ("Cannot call " ^ show_literal l ^ ". Not a function"))

let rec interpret_stmt = function
  (* This returns either Some, if we hit a return statement
   * or None *)
  | Expr e ->
      ignore (interpret_expr e);
      None
  | Print e ->
      print_endline (interpret_expr e |> show_literal);
      None
  | Block decls -> interpret_block decls
  | If (expr, then', else') -> interpret_if expr then' else'
  | While (expr, stmt) -> interpret_while expr stmt
  | Return expr -> (
      match expr with
      | Some expr -> Some (interpret_expr expr)
      | None -> Some Nil)

and interpret_block decls =
  let rec do_until lst ret =
    if Option.is_some ret then ret
    else
      match lst with
      | [] -> None
      | Stmt s :: tail ->
          let ret = interpret_stmt s in
          do_until tail ret
      | decl :: decls ->
          interpret_decl decl;
          do_until decls ret
  in
  env := Environment.open_block !env;
  let ret = do_until decls None in
  env := Environment.close_block !env;
  ret

and interpret_if expr then' else' =
  match interpret_expr expr with
  | Bool bool -> (
      if bool then interpret_stmt then'
      else match else' with Some else' -> interpret_stmt else' | None -> None)
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
  let rec do_while () =
    if condition () then
      match interpret_stmt stmt with
      | Some ret -> Some ret
      | None -> do_while ()
    else None
  in
  do_while ()

and interpret_decl = function
  | Var_decl (name, expr) ->
      let expr =
        match expr with Some expr -> interpret_expr expr | None -> Nil
      in
      env := Environment.add ~name expr !env
  | Stmt s -> ignore (interpret_stmt s)
  | Fun_decl { name; parameters; body } ->
      interpret_fun_decl name parameters body

and interpret_fun_decl name params body =
  let closure = ref !env in

  let call arguments =
    (* We create a new block with the parameter bindings.
     * This is not really correct, but good enough for now *)
    let saved_env = !env in
    env := Environment.open_block !closure;

    (try
       List.iter2
         (fun name argument -> env := Environment.add ~name argument !env)
         params arguments
     with Invalid_argument _ ->
       raise
         (RuntimeError
            ("Wrong arity: Expected "
            ^ (List.length params |> string_of_int)
            ^ " arguments")));

    let ret = interpret_stmt body in

    (* Save the changes done to the closere environment *)
    closure := Environment.close_block !env;
    env := saved_env;

    match ret with Some ret -> ret | None -> Nil
  in

  (* Should use the global scope? We differ from the book here *)
  env := Environment.add ~name (Fun { name; call }) !env;
  (* We add the function to the closure for recursion *)
  closure := Environment.add ~name (Fun { name; call }) !closure

let interpret = List.iter interpret_decl
