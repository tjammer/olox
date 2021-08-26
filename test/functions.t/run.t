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

Closure tests from reference repo
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

Function tests from reference repo

  $ dune exec -- olox body_must_be_block.lox
  :3:12: syntax error
  $ dune exec -- olox empty_body.lox
  Nil
  $ dune exec -- olox extra_arguments.lox
  RuntimeError: Wrong arity: Expected 2 arguments
  $ dune exec -- olox local_mutual_recursion.lox
  StaticError: Variable 'isOdd' does not exist
  $ dune exec -- olox local_recursion.lox
  (Number 21.)
  $ dune exec -- olox missing_arguments.lox
  RuntimeError: Wrong arity: Expected 2 arguments
  $ dune exec -- olox missing_comma_in_parameters.lox
  :3:15: syntax error
  $ dune exec -- olox mutual_recursion.lox
  (Bool true)
  (Bool true)
  $ dune exec -- olox nested_call_with_arguments.lox
  (String "hello world")
  $ dune exec -- olox parameters.lox
  (Number 0.)
  (Number 1.)
  (Number 3.)
  (Number 6.)
  (Number 10.)
  (Number 15.)
  (Number 21.)
  (Number 28.)
  (Number 36.)
  $ dune exec -- olox print.lox
  (Fun { callable = "foo"; call = <fun> })
  (Fun { callable = "clock"; call = <fun> })
  $ dune exec -- olox recursion.lox
  (Number 21.)
  $ dune exec -- olox too_many_arguments.lox
  Fatal error: exception Sys_error("too_many_arguments.lox: No such file or directory")
  [2]
  $ dune exec -- olox too_many_parameters.lox
  Fatal error: exception Sys_error("too_many_parameters.lox: No such file or directory")
  [2]
