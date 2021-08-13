We test functions and their return values

  $ dune exec -- olox add.lox
  (Ast.Fun <fun>)

  $ dune exec -- olox hi.lox
  (Ast.String "Hi, Dear Reader!")

Here we just make sure we don't crash. The time will be different on each run

  $ dune exec -- olox clock.lox
