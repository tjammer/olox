(* We don't need a resolver b/c we use a persistent data structure
   for environment, but having some static analysis is nice *)

open Ast
open Containers
module SymbolTbl = Hashtbl.Make (String)

exception StaticError of string

type symbol = { defined : bool; used : bool }

type env_kind = Outside | In_func | In_method of bool | In_init of bool
(* The bool is true if inside a subclass *)

type env_state = { def : symbol SymbolTbl.t; kind : env_kind }

type t = env_state list

let create () =
  let globals = SymbolTbl.create 4 in
  (* We need to make sure the glabal functions are available.
   * In contrast to jlox, we resolve all symbols here,
   * including globals *)
  SymbolTbl.add globals "clock" { defined = true; used = true };
  [ { def = globals; kind = Outside } ]

let ( %. ) = Fun.flip

let empty_scope () = failwith "Internal_error: Empty scope stack"

let rec toplevel_return ~expr ?(inside = false) = function
  | scope :: scopes -> (
      match scope.kind with
      | In_init _ when expr ->
          raise (StaticError "Can't return a value from an initializer")
      | In_func | In_method _ | In_init _ ->
          toplevel_return ~expr ~inside:true scopes
      | Outside -> toplevel_return ~expr ~inside scopes)
  | [] ->
      if inside then ()
      else raise (StaticError "Can't return from toplevel code")

let rec outside_class_this = function
  | scope :: scopes -> (
      match scope.kind with
      | In_method _ | In_init _ -> ()
      | In_func | Outside -> outside_class_this scopes)
  | [] -> raise (StaticError "Can't use 'this' outside of a class")

let rec has_superclass = function
  | scope :: scopes -> (
      match scope.kind with
      | In_method true | In_init true -> ()
      | In_method false | In_func | In_init false | Outside ->
          has_superclass scopes)
  | [] -> raise (StaticError "Can't use 'super' in a non-subclass")

let begin_scope ?(kind = Outside) data =
  { def = SymbolTbl.create 4; kind } :: data

let end_scope = function
  | [] -> empty_scope ()
  | scope :: scopes ->
      (* We check if all variables in the scope have been used.
         Otherwise, we throw *)
      SymbolTbl.iter
        (fun name symbol ->
          if not symbol.used then
            raise (StaticError (Printf.sprintf "Unused variable: %s" name)))
        scope.def;
      scopes

(* We start with the more interesting nodes for the resolver in the book *)
let rec resolve_block data decls =
  begin_scope data |> List.fold_left resolve_decl %. decls |> end_scope

and resolve_decl data = function
  | Var_decl (name, expr) -> resolve_var_decl data name expr
  | Fun_decl { name; parameters; body } ->
      resolve_fun_decl data name parameters body
  | Stmt stmt -> resolve_stmt data stmt
  | Class_decl (name, methods, super) ->
      resolve_class_decl data name methods super

and resolve_var_decl data name expr =
  declare data name
  |> (fun data ->
       match expr with Some expr -> resolve_expr data expr | None -> data)
  |> define name

and resolve_fun_decl data name parameters body =
  let data = declare data name |> define name in
  resolve_fun data parameters body

and resolve_class_decl data name methods super =
  let handle_super data =
    match super with
    | Some cl ->
        (* If there is a superclass, we resolve it.
           We raise if the class tries to inherit from itself *)
        if String.equal cl name then
          raise (StaticError ("A class can't inherit from itself: " ^ name));
        resolve cl data;
        (In_method true, data)
    | None -> (In_method false, data)
  in

  declare data name |> define name |> handle_super |> fun (kind, data) ->
  begin_scope ~kind data
  |> List.fold_left (fun data func ->
         if String.(func.name = "init") then
           let kind =
             match (List.hd data).kind with
             | In_method b | In_init b -> In_init b
             | _ -> failwith "Internal error: In subclass check"
           in
           resolve_fun ~kind data func.parameters func.body
         else resolve_fun data func.parameters func.body)
     %. methods
  |> end_scope

and resolve_fun ?(kind = In_func) data parameters body =
  begin_scope ~kind data
  |> List.fold_left (fun data name -> declare data name |> define name)
     %. parameters
  |> resolve_stmt %. body |> end_scope

and resolve_stmt data = function
  | Block decls -> resolve_block data decls
  | Expr expr -> resolve_expr data expr
  | If (expr, then', else') -> (
      resolve_expr data expr |> resolve_stmt %. then' |> fun data ->
      match else' with Some stmt -> resolve_stmt data stmt | None -> data)
  | Print expr -> resolve_expr data expr
  | Return None ->
      toplevel_return ~expr:false data;
      data
  | Return (Some expr) ->
      toplevel_return ~expr:true data;
      resolve_expr data expr
  | While (expr, stmt) -> resolve_expr data expr |> resolve_stmt %. stmt

and declare data name =
  match data with
  | [] -> empty_scope ()
  | scope :: _ ->
      SymbolTbl.replace scope.def name { defined = false; used = false };
      data

and define name data =
  match data with
  | [] -> empty_scope ()
  | scope :: _ ->
      (match SymbolTbl.find_opt scope.def name with
      | Some symbol ->
          SymbolTbl.replace scope.def name { symbol with defined = true }
      | None -> failwith "Internal error: Define should be called after declare");
      data

and resolve_expr data = function
  | Primary primary -> resolve_primary data primary
  | Assign (name, expr) ->
      resolve_expr data expr |> resolve_assign_target %. name
  | Unary un -> resolve_expr data un.expr
  | Binary bin -> resolve_expr data bin.left |> resolve_expr %. bin.right
  | Logical (_, left, right) -> resolve_expr data left |> resolve_expr %. right
  | Call (name, exprs) ->
      resolve_expr data name |> List.fold_left resolve_expr %. exprs
  | Class_get (instance, _) ->
      (* We don't resolve the field, b/c we want to be able to dynamically add data *)
      resolve_expr data instance

and resolve_primary data = function
  | Value value -> resolve_value data value
  | Grouping expr -> resolve_expr data expr

and resolve_value data = function
  | Identifier name -> (
      match data with
      | [] -> empty_scope ()
      | scope :: _ -> (
          match SymbolTbl.find_opt scope.def name with
          (* I don't think this should be an error, but we go with the book *)
          | Some { defined = false; used = _ } ->
              raise
                (StaticError
                   ("Can't read local variable in its own initializer: " ^ name))
          | None | Some { defined = true; used = _ } ->
              resolve name data;

              data))
  | This ->
      outside_class_this data;
      data
  | Super _ ->
      has_superclass data;
      data
  | Method _ | Class _ | Instance _ | Number _ | String _ | Bool _ | Nil | Fun _
    ->
      data

and resolve_assign_target data = function
  (* So far, we only allow plain identifiers *)
  | Primary (Value (Identifier name)) ->
      resolve name data;
      data
  | Class_get (instance, _) -> resolve_expr data instance
  | expr ->
      raise
        (StaticError
           ("Assignment only allowed to identifiers, not: " ^ show_expr expr))

and resolve name data =
  let rec aux i = function
    | [] -> raise (StaticError ("Variable '" ^ name ^ "' does not exist"))
    | scope :: scopes -> (
        match SymbolTbl.find_opt scope.def name with
        | None -> aux (i + 1) scopes
        | Some _ ->
            (* The variable had to be resolved, so we mark it used *)
            SymbolTbl.replace scope.def name { defined = true; used = true })
  in
  aux 0 data

let make decls =
  match List.fold_left resolve_decl (create ()) decls with
  | [ _ ] as scope -> end_scope scope
  | [] -> failwith "Internal error: Where is the last scope?"
  | _ :: _ :: _ -> failwith "Internal error: Some scopes are not closed"
