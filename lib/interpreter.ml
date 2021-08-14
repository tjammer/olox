open Ast

exception RuntimeError of string

let envi = ref Environment.globals

let rec interpret_expr = function
  (* Since olox is in imperative language, an expression can update the env
   * as a side effect. We use a ref for that *)
  | Primary (Literal (Identifier id)) -> (
      match Environment.find ~id !envi with
      | Some l -> l
      | None -> raise (RuntimeError ("Cannot find variable '" ^ id ^ "'")))
  | Primary (Literal l) -> l
  | Unary { op; expr } -> interpret_unop op expr
  | Binary { left; op; right } -> interpret_binop op left right
  | Primary (Grouping e) -> interpret_expr e
  | Assign (id, expr) ->
      let value = interpret_expr expr in
      envi := Environment.replace ~id value !envi;
      value
  | Logical (op, left, right) -> interpret_logicalop op left right
  | Call (func, arguments) -> interpret_call func arguments

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
  envi := Environment.open_block !envi;
  let ret = do_until decls None in
  envi := Environment.close_block !envi;
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
  | Var_decl (id, expr) ->
      let expr =
        match expr with Some expr -> interpret_expr expr | None -> Nil
      in
      envi := Environment.add ~id expr !envi
  | Stmt s -> ignore (interpret_stmt s)
  | Fun_decl { name; parameters; body } ->
      interpret_fun_decl name parameters body

and interpret_fun_decl name params body =
  let closure = ref !envi in
  let closure_len = List.length !closure in
  let call arguments =
    (* We create a new block with the parameter bindings.
     * This is not really correct, but good enough for now *)
    envi := Environment.open_block (!closure @ !envi);

    (try
       List.iter2
         (fun id argument -> envi := Environment.add ~id argument !envi)
         params arguments
     with Invalid_argument _ ->
       raise
         (RuntimeError
            ("Wrong arity: Expected "
            ^ (List.length params |> string_of_int)
            ^ " arguments")));

    let ret = interpret_stmt body in

    (* Save the changes done to the closere environment *)
    let new_closure, env =
      CCList.take_drop closure_len (Environment.close_block !envi)
    in
    closure := new_closure;
    envi := env;

    match ret with Some ret -> ret | None -> Nil
  in

  (* Should use the global scope? We differ from the book here *)
  envi := Environment.add ~id:name (Fun { name; call }) !envi

let interpret = List.iter interpret_decl
