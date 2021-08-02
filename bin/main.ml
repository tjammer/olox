let () =
  if Array.length Sys.argv > 2 then (
    print_endline "Usage: olox [script]";
    exit 64)
  else if Array.length Sys.argv = 2 then Olox.run_file Sys.argv.(1)
  else Olox.run_prompt ()
