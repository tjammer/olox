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

let add ~name value = function
  | [] ->
      (* We print a warning, b/c this should never happen.
       * It's not actually a problem though *)
      prerr_endline "Internal error: There is an empty environment. How?";
      [ StrMap.(add name value StrMap.empty) ]
  | env :: envs -> StrMap.(add name value env) :: envs

let replace ~name value env =
  let rec aux ~name value head = function
    | [] -> raise (RuntimeError ("Variable " ^ name ^ " does not exist"))
    | env :: envs -> (
        match StrMap.find_opt name env with
        | Some _ ->
            let env = StrMap.(add name value env) in
            List.rev head @ (env :: envs)
        | None -> aux ~name value (env :: head) envs)
  in
  aux ~name value [] env

let find ~name =
  let rec aux ~name = function
    | [] -> None
    | env :: envs -> (
        match StrMap.find_opt name env with
        | Some value -> Some value
        | None -> aux ~name envs)
  in
  aux ~name

let open_block env = List.cons StrMap.empty env

let close_block = function
  | [] ->
      (* As above, this should never happen. *)
      prerr_endline "Internal error: There is an empty environment. How?";
      empty
  | _ :: env -> env
