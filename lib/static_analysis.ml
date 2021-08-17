(* We don't need a resolver b/c we use a persistent data structure
   for environment, but having some static analysis is nice *)

open Ast
open Containers
module SymbolTbl = Hashtbl.Make (String)

exception StaticError of string

type symbol = { defined : bool; used : bool }

type t = { def : symbol SymbolTbl.t list }

let create () =
  let globals = SymbolTbl.create 4 in
  (* We need to make sure the glabal functions are available.
   * In contrast to jlox, we resolve all symbols here,
   * including globals *)
  SymbolTbl.add globals "clock" { defined = true; used = true };

  { def = [ globals ] }

let ( %. ) = Fun.flip

let empty_scope () = failwith "Internal_error: Empty scope stack"

(* We start with the more interesting nodes for the resolver in the book *)
let rec resolve_block data decls =
  begin_scope data |> List.fold_left resolve_decl %. decls |> end_scope

and begin_scope { def } = { def = SymbolTbl.create 4 :: def }

and end_scope { def } =
  match def with
  | [] -> empty_scope ()
  | scope :: def ->
      (* We check if all variables in the scope have been used.
         Otherwise, we throw *)
      SymbolTbl.iter
        (fun name symbol ->
          if not symbol.used then
            raise (StaticError (Printf.sprintf "Unused variable: %s" name)))
        scope;
      { def }

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
  match data.def with
  | [] -> empty_scope ()
  | scope :: _ ->
      SymbolTbl.replace scope name { defined = false; used = false };
      data

and define name data =
  match data.def with
  | [] -> empty_scope ()
  | scope :: _ ->
      (match SymbolTbl.find_opt scope name with
      | Some symbol ->
          SymbolTbl.replace scope name { symbol with defined = true }
      | None -> failwith "Internal error: Define should be called after declare");
      data

and resolve_expr data = function
  | Primary primary -> resolve_primary data primary
  | Assign (name, expr) ->
      let data = resolve_expr data expr in
      resolve name data;
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
      match data.def with
      | [] -> empty_scope ()
      | scope :: _ -> (
          match SymbolTbl.find_opt scope name with
          (* I don't think this should be an error, but we go with the book *)
          | Some { defined = false; used = _ } ->
              raise
                (StaticError
                   ("Can't read local variable in its own initializer: " ^ name))
          | None | Some { defined = true; used = _ } ->
              resolve name data;

              data))
  | Number _ | String _ | Bool _ | Nil | Fun _ -> data

and resolve name data =
  let rec aux i = function
    | [] -> raise (StaticError ("Variable '" ^ name ^ "' does not exist"))
    | scope :: scopes -> (
        match SymbolTbl.find_opt scope name with
        | None -> aux (i + 1) scopes
        | Some _ ->
            (* The variable had to be resolved, so we mark it used *)
            SymbolTbl.replace scope name { defined = true; used = true })
  in
  aux 0 data.def

let make decls =
  match List.fold_left resolve_decl (create ()) decls with
  | { def = [ _ ] } as scope -> end_scope scope
  | { def = [] } -> failwith "Internal error: Where is the last scope?"
  | { def = _ :: _ } -> failwith "Internal error: Some scopes are not closed"
