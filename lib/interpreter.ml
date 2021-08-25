open Ast
open Containers

exception RuntimeError of string

let env = ref Globals.globals

let rec find_init = function
  | [] -> None
  | (name, meth) :: methods ->
      if String.(name = "init") then Some meth else find_init methods
(* To find the init method for instance construction *)

let rec interpret_expr = function
  (* Since olox is in imperative language, an expression can update the env
   * as a side effect. We use a ref for that *)
  | Primary primary -> interpret_primary primary
  | Unary { op; expr } -> interpret_unop op expr
  | Binary { left; op; right } -> interpret_binop op left right
  | Assign (name, expr) -> interpret_assign name expr
  | Logical (op, left, right) -> interpret_logicalop op left right
  | Call (func, arguments) -> interpret_call func arguments
  | Class_get (instance, field) -> interpret_get instance field

and interpret_primary = function
  | Value (Identifier name) -> (
      match Environment.find ~name !env with
      | Some v -> !v
      | None -> raise (RuntimeError ("Cannot find variable '" ^ name ^ "'")))
  | Value This -> (
      let name = "this" in
      match Environment.find ~name !env with
      | Some v -> !v
      | None -> raise (RuntimeError ("Cannot find variable '" ^ name ^ "'")))
  | Value v -> v
  | Grouping e -> interpret_expr e

and interpret_assign name expr =
  match name with
  | Primary (Value (Identifier name)) ->
      let value = interpret_expr expr in
      (* env := Environment.replace ~name value !env; *)
      (match Environment.find ~name !env with
      | Some v -> v := value
      | None -> failwith "TODO");
      value
  | Class_get (name, field) -> (
      match interpret_expr name with
      | Instance (_, inst_env) ->
          let value = interpret_expr expr in
          inst_env := Environment.add ~name:field value !inst_env;
          value
      | value ->
          raise
            (RuntimeError
               ("Assignment target with get expression must be instance, not: "
              ^ show_value value)))
  | _ ->
      failwith
        ("Internal Error: Assignment to wrong target not caught in static \
          analysis: " ^ show_expr name)

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
           ^ show_value (interpret_expr left)
           ^ " and "
           ^ show_value (interpret_expr right)))

and interpret_equal left right =
  match (left, right) with
  | Bool l, Bool r -> Bool (Bool.equal l r)
  | String l, String r -> Bool (String.equal l r)
  | Number l, Number r -> Bool (Float.equal l r)
  | Nil, Nil -> Bool true
  | _, Nil | Nil, _ -> Bool false
  | _ ->
      raise
        (RuntimeError
           ("Cannot check equality between " ^ show_value left ^ " and "
          ^ show_value right))

and interpret_call func args =
  (* Arity check happens in the closure *)
  match interpret_expr func with
  | Fun func -> func.call (List.map interpret_expr args)
  | Class (name, methods) -> (
      (* We create a class instance *)
      let instance =
        (* We store the methods as functions in the instance.
           Otherwise, we'd need to either copy the whole class or
           store the reference to the class somewhere *)
        let methods =
          List.fold_left
            (fun env (name, f) ->
              env := Environment.add ~name (Method f) !env;
              env)
            (ref Environment.empty) methods
        in
        Instance (name, methods)
      in
      match find_init methods with
      | Some init ->
          ignore ((init (Some instance)).call (List.map interpret_expr args));
          instance
      | None -> instance)
  | l ->
      raise (RuntimeError ("Cannot call " ^ show_value l ^ ". Not a function"))

and interpret_get instance field =
  match interpret_expr instance with
  | Instance (name, env) as i -> (
      match Environment.find ~name:field !env with
      | Some (Method m) ->
          let f = m (Some i) in
          (* If the method is 'init' it's an initializer.
           * In that case, we return the instance *)
          if String.(f.callable = "init") then
            Fun
              {
                f with
                call =
                  (fun params ->
                    ignore (f.call params);
                    i);
              }
          else Fun f
      | Some value -> value
      | None ->
          raise (RuntimeError ("Undefined property on " ^ name ^ ": " ^ field)))
  | _ -> raise (RuntimeError "Only instances have properties")

let rec interpret_stmt = function
  (* This returns either Some, if we hit a return statement
   * or None *)
  | Expr e ->
      ignore (interpret_expr e);
      None
  | Print e ->
      print_endline (interpret_expr e |> show_value);
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
      env := Environment.add ~name (ref expr) !env
  | Stmt s -> ignore (interpret_stmt s)
  | Fun_decl { name; parameters; body } ->
      (* None in the following call means: Do not bind anything to 'this' *)
      ignore
        (let f, _ = interpret_method name parameters body in
         f None)
  | Class_decl (name, funcs) -> interpret_class_decl name funcs

and interpret_method name params body =
  (* Here, method is a function which takes a instance option and returns a
     callable. For normal function, we simply don't pass an instance.
     For class methods, the instance we pass is bound to 'this' *)
  let closure = ref !env in

  let ret instance =
    let call arguments =
      (* We create a new block with the parameter bindings.
       * This is not really correct, but good enough for now *)
      let saved_env = !env in
      env := Environment.open_block !closure;
      (* Here, we bind 'this' to the class instance *)
      (match instance with
      | Some instance -> env := Environment.add ~name:"this" (ref instance) !env
      | None -> ());

      (try
         List.iter2
           (fun name argument ->
             env := Environment.add ~name (ref argument) !env)
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

    let func = { callable = name; call } in
    (* Restore the global environment *)
    env := Environment.add ~name (ref (Fun func)) !env;
    (* We add the function to the closure for recursion *)
    closure := Environment.add ~name (ref (Fun func)) !closure;
    func
  in
  (* We return the closure to add the Class to it *)
  (ret, closure)

and interpret_class_decl name funcs =
  let funcs, closures =
    List.map
      (fun func ->
        let f, closure = interpret_method func.name func.parameters func.body in
        ((func.name, f), closure))
      funcs
    |> List.split
  in
  List.iter
    (fun closure ->
      closure := Environment.add ~name (ref (Class (name, funcs))) !closure)
    closures;
  env := Environment.add ~name (ref (Class (name, funcs))) !env

let interpret = List.iter interpret_decl
