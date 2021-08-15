We test functions and their return values

  $ dune exec -- olox add.lox
  Ast.Fun {name = "add"; call = <fun>}

  $ dune exec -- olox hi.lox
  (Ast.String "Hi, Dear Reader!")

Here we just make sure we don't crash. The time will be different on each run

  $ dune exec -- olox clock.lox

Return statement

  $ dune exec -- olox return.lox
  (Ast.Number 1.)
  (Ast.Number 2.)
  (Ast.String "done")
  (Ast.Number 3.)

Fibonacci example

  $ dune exec -- olox fib.lox
  (Ast.Number 0.)
  (Ast.Number 1.)
  (Ast.Number 1.)
  (Ast.Number 2.)
  (Ast.Number 3.)
  (Ast.Number 5.)
  (Ast.Number 8.)
  (Ast.Number 13.)
  (Ast.Number 21.)
  (Ast.Number 34.)
  (Ast.Number 55.)
  (Ast.Number 89.)
  (Ast.Number 144.)
  (Ast.Number 233.)
  (Ast.Number 377.)
  (Ast.Number 610.)
  (Ast.Number 987.)
  (Ast.Number 1597.)
  (Ast.Number 2584.)
  (Ast.Number 4181.)

Closure

  $ dune exec -- olox closure.lox
  (Ast.Number 1.)
  (Ast.Number 2.)

The function should not see variables which are declared in the future

  $ dune exec -- olox closure_dont_overwrite.lox
  (Ast.String "global")
  (Ast.String "global")
