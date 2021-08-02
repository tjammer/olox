module Scanner = Scanner

let report line where message =
  Format.eprintf "[line %i] Error%s: %s" line where message

let run src =
  let tokens = Scanner.tokenize src in
  List.iter (fun token -> Format.printf "%a\n%!" Scanner.Token.pp token) tokens;
  Ok ()

let run_file filename =
  let content = CCIO.(with_in filename read_all) in
  match run content with
  | Ok _ -> ()
  | Error (line, message) -> report line "" message

let rec run_prompt () =
  try
    print_string "> ";
    ignore @@ run (read_line ());
    run_prompt ()
  with End_of_file -> ()
