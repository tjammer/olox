All these tests have unused variables

  $ dune exec -- olox unused_free_var.lox
  StaticError: Unused variable: a

  $ dune exec -- olox unused_fun.lox
  StaticError: Unused variable: unused

  $ dune exec -- olox initialize_self.lox
  StaticError: Can't read local variable in its own initializer: a

No return at top level

  $ dune exec -- olox toplevel_return.lox
  StaticError: Can't return from toplevel code
