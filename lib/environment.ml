module StrMap = Map.Make (String)

exception RuntimeError of string

type t = Ast.literal StrMap.t list

let globals : t =
  let clock params =
    (* We need to check the arity ourselven *)
    match params with
    | [] ->
        let t = Mtime_clock.elapsed () in
        Ast.Number (Mtime.Span.to_ms t)
    | _ -> raise (RuntimeError "Wrong arity: Expected 0 arguments")
  in
  [ StrMap.add "clock" (Ast.Fun { name = "clock"; call = clock }) StrMap.empty ]

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

let open_block : t -> t = List.cons StrMap.empty

let close_block = function
  | [] ->
      (* As above, this should never happen. *)
      prerr_endline "Internal error: There is an empty environment. How?";
      empty
  | _ :: env -> env
