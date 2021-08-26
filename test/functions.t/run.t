We test functions and their return values

  $ dune exec -- olox add.lox
  (Fun { callable = "add"; call = <fun> })

  $ dune exec -- olox hi.lox
  (String "Hi, Dear Reader!")

Here we just make sure we don't crash. The time will be different on each run

  $ dune exec -- olox clock.lox

Return statement

  $ dune exec -- olox return.lox
  (Number 1.)
  (Number 2.)
  (String "done")
  (Number 3.)

Fibonacci example

  $ dune exec -- olox fib.lox
  (Number 0.)
  (Number 1.)
  (Number 1.)
  (Number 2.)
  (Number 3.)
  (Number 5.)
  (Number 8.)
  (Number 13.)
  (Number 21.)
  (Number 34.)
  (Number 55.)
  (Number 89.)
  (Number 144.)
  (Number 233.)
  (Number 377.)
  (Number 610.)
  (Number 987.)
  (Number 1597.)
  (Number 2584.)
  (Number 4181.)

Closure

  $ dune exec -- olox closure.lox
  (Number 1.)
  (Number 2.)

The function should not see variables which are declared in the future

  $ dune exec -- olox closure_dont_overwrite.lox
  (String "global")
  (String "global")

From reference repo tests
  $ dune exec -- olox assign_to_closure.lox
  (String "local")
  (String "after f")
  (String "after f")
  (String "after g")
  $ dune exec -- olox assign_to_shadowed_later.lox
  (String "inner")
  (String "assigned")
  $ dune exec -- olox closed_closure_in_function.lox
  (String "local")
  $ dune exec -- olox close_over_function_parameter.lox
  (String "param")
  $ dune exec -- olox close_over_later_variable.lox
  (String "b")
  (String "a")
  $ dune exec -- olox close_over_method_parameter.lox
  (String "param")
  $ dune exec -- olox nested_closure.lox
  (String "a")
  (String "b")
  (String "c")
  $ dune exec -- olox open_closure_in_function.lox
  (String "local")
  $ dune exec -- olox reference_closure_multiple_times.lox
  (String "a")
  (String "a")
  $ dune exec -- olox reuse_closure_slot.lox
  (String "a")
  $ dune exec -- olox shadow_closure_with_local.lox
  (String "closure")
  (String "shadow")
  (String "closure")
