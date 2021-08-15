(* We don't need a resolver b/c we use a persistent data structure for environment,
   but having some static analyis is nice *)

open Ast
open Containers
module UndefinedTbl = Hashtbl.Make (String)

exception StaticError of string

(* type t = { undef : bool UndefinedTbl.t list; max_defs : string list } *)

type t = bool UndefinedTbl.t list

let ( %. ) = Fun.flip

let empty_scope () = failwith "Internal_error: Empty scope stack"

(* We start with the more interesting nodes for the resolver in the book *)
let rec resolve_block data decls =
  begin_scope data |> List.fold_left resolve_decl %. decls |> end_scope

and begin_scope data = UndefinedTbl.create 4 :: data

and end_scope = function
  | [] -> empty_scope ()
  | _ :: data ->
      Printf.printf "static: %i\n" (List.length data);
      data

and resolve_decl data = function
  | Var_decl (name, expr) -> resolve_var_decl data name expr
  | Fun_decl { name; parameters; body } ->
      resolve_fun_decl data name parameters body
  | Stmt stmt -> resolve_stmt data stmt

and resolve_var_decl data name expr =
  declare data name
  |> (fun data ->
       match expr with Some expr -> resolve_expr data expr | None -> data)
  |> define name

and resolve_fun_decl data name parameters body =
  let data = declare data name |> define name in
  resolve_fun data parameters body

and resolve_fun data parameters body =
  begin_scope data
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
  | Return None -> data
  | Return (Some expr) -> resolve_expr data expr
  | While (expr, stmt) -> resolve_expr data expr |> resolve_stmt %. stmt

and declare data name =
  match data with
  | [] -> empty_scope ()
  | scope :: _ as data ->
      UndefinedTbl.replace scope name false;
      data

and define name data =
  match data with
  | [] -> empty_scope ()
  | scope :: _ as data ->
      UndefinedTbl.replace scope name true;
      data

and resolve_expr data = function
  | Primary primary -> resolve_primary data primary
  | Assign (name, expr) ->
      let data = resolve_expr data expr in
      ignore (resolve name data);
      data
  | Unary un -> resolve_expr data un.expr
  | Binary bin -> resolve_expr data bin.left |> resolve_expr %. bin.right
  | Logical (_, left, right) -> resolve_expr data left |> resolve_expr %. right
  | Call (name, exprs) ->
      resolve_primary data name |> List.fold_left resolve_expr %. exprs

and resolve_primary data = function
  | Literal lit -> resolve_literal data lit
  | Grouping expr -> resolve_expr data expr

and resolve_literal data = function
  | Identifier name -> (
      match data with
      | [] -> empty_scope ()
      | scope :: _ as data -> (
          match UndefinedTbl.find_opt scope name with
          (* I don't think this should be an error, but we go with the book *)
          | Some false ->
              raise
                (StaticError "Can't read local variable in its own initializer")
          | None | Some true ->
              ignore (resolve name data);
              (* TODO use the result *) data))
  | Number _ | String _ | Bool _ | Nil | Fun _ -> data

and resolve name data =
  let rec aux i = function
    | [] -> raise (StaticError ("Variable " ^ " does not exist"))
    | scope :: scopes -> (
        match UndefinedTbl.find_opt scope name with
        | None -> aux (i + 1) scopes
        | Some _ -> i)
  in
  aux 0 data

let make decls = List.fold_left resolve_decl [UndefinedTbl.create 4] decls
