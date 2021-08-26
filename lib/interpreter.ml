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
  | Value (Super id) -> interpret_super id
  | Value v -> v
  | Grouping e -> interpret_expr e

and interpret_super id =
  (* Get the instance to get the superclass *)
  match Environment.find ~name:"this" !env with
  | Some ({ contents = Instance (_, _, cl) } as i) -> (
      match cl.super with
      | Some cl -> (
          match class_get_method cl id with
          | Some meth -> Fun (meth (Some !i))
          | None ->
              raise
                (RuntimeError
                   (Printf.sprintf "Method %s does not exist on class %s" id
                      cl.cl_name)))
      | None -> failwith "Internal error: Should have been caught before")
  | _ -> failwith "Internal error: super should use instance"

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
      | Instance (_, inst_env, _) ->
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
  | Class c -> (
      (* We create a class instance *)
      let instance =
        (* We store the methods as functions in the instance.
           Otherwise, we'd need to either copy the whole class or
           store the reference to the class somewhere *)
        (* TODO delete *)
        (* let methods =
         *   List.fold_left
         *     (fun env (name, f) ->
         *       env := Environment.add ~name (Method f) !env;
         *       env)
         *     (ref Environment.empty) c.methods
         * in *)
        Instance (c.cl_name, ref Environment.empty, c)
      in
      match find_init c.methods with
      | Some init ->
          ignore ((init (Some instance)).call (List.map interpret_expr args));
          instance
      | None -> instance)
  | l ->
      raise (RuntimeError ("Cannot call " ^ show_value l ^ ". Not a function"))

and interpret_get instance field =
  match interpret_expr instance with
  | Instance (name, env, cl) as i -> (
      match Environment.find ~name:field !env with
      | Some value -> value
      | None -> (
          match class_get_method cl field with
          | Some m ->
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
          | None ->
              raise
                (RuntimeError ("Undefined property on " ^ name ^ ": " ^ field)))
      )
  | _ -> raise (RuntimeError "Only instances have properties")

and class_get_method cl field =
  match List.Assoc.get ~eq:String.equal field cl.methods with
  | Some meth -> Some meth
  | None -> (
      (* Look in the superclass if it exists *)
      match cl.super with Some cl -> class_get_method cl field | None -> None)

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
  | Class_decl (name, funcs, super) -> interpret_class_decl name funcs super

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

and interpret_class_decl name funcs super =
  let methods, closures =
    List.map
      (fun func ->
        let f, closure = interpret_method func.name func.parameters func.body in
        ((func.name, f), closure))
      funcs
    |> List.split
  in
  (* Add the class name itself to the method closure.
     Allows methods to call the ctor *)
  List.iter
    (fun closure ->
      closure :=
        Environment.add ~name
          (ref (Class { cl_name = name; methods; super = None }))
          !closure)
    closures;

  let super =
    match super with
    | Some super -> (
        match interpret_primary (Value (Identifier super)) with
        | Class cl -> Some cl
        | _ -> raise (RuntimeError ("Superclass must be a class: " ^ super)))
    | None -> None
  in

  env :=
    Environment.add ~name (ref (Class { cl_name = name; methods; super })) !env

let interpret = List.iter interpret_decl
