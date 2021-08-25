module StrMap = Environment.StrMap

exception RuntimeError of string

let globals =
  let clock params =
    (* We need to check the arity ourselven *)
    match params with
    | [] ->
        let t = Mtime_clock.elapsed () in
        Ast.Number (Mtime.Span.to_ms t)
    | _ -> raise (RuntimeError "Wrong arity: Expected 0 arguments")
  in
  [
    StrMap.add "clock"
      (ref (Ast.Fun { callable = "clock"; call = clock }))
      StrMap.empty;
  ]
