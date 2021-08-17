All these tests have unused variables

  $ dune exec -- olox unused_free_var.lox
  Fatal error: exception Olox__Static_analysis.StaticError("Unused variable: a")
  [2]

  $ dune exec -- olox unused_fun.lox
  Fatal error: exception Olox__Static_analysis.StaticError("Unused variable: unused")
  [2]

  $ dune exec -- olox initialize_self.lox
  Fatal error: exception Olox__Static_analysis.StaticError("Can't read local variable in its own initializer: a")
  [2]
