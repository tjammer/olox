We can declare a class

  $ dune exec -- olox decl.lox
  (Ast.Class "DevonshireCream")
  (Ast.Instance "DevonshireCream")
  RuntimeError: Undefined property on DevonshireCream: field

We can access and define fields

  $ dune exec -- olox fields.lox
  (Ast.String "too sweet")
  (Ast.Number 12.)
  (Ast.Number 42.)
  RuntimeError: Assignment target with get expression must be instance, not: (Ast.Class "Bagel")

Call methods

  $ dune exec -- olox methods.lox
  (Ast.String "Crunch crunch crunch!")
  (Ast.String "Crunch crunch crunch!")

Bound methods and 'this'

  $ dune exec -- olox this_bound_methods.lox
  (Ast.String "Jane")
  (Ast.Instance "Egotist")
  (Ast.String "The German chocolate cake is delicious!")

'This' outside class is not allowed

  $ dune exec -- olox outside_class_this.lox
  Fatal error: exception Olox__Static_analysis.StaticError("Can't use 'this' outside of a class")
  [2]

Initializers

  $ dune exec -- olox init.lox
  (Ast.String "init")
  (Ast.String "directly")
  (Ast.Instance "Foo")
  (Ast.Number 10.)
  (Ast.Instance "Foo")
  (Ast.Instance "ReturnInInit")

Return with expression in initializer

  $ dune exec -- olox return_from_init.lox
  Fatal error: exception Olox__Static_analysis.StaticError("Can't return a value from an initializer")
  [2]

Tests from reference repo

  $ dune exec -- olox empty.lox
  (Ast.Class "Foo")
  $ dune exec -- olox inherit_self.lox
  Fatal error: exception Olox__Static_analysis.StaticError("A class can't inherit from itself: Foo")
  [2]
  $ dune exec -- olox inherited_method.lox
  (Ast.String "in foo")
  (Ast.String "in bar")
  (Ast.String "in baz")
  $ dune exec -- olox local_inherit_other.lox
  (Ast.Class "B")
  $ dune exec -- olox local_inherit_self.lox
  Fatal error: exception Olox__Static_analysis.StaticError("A class can't inherit from itself: Foo")
  [2]
  $ dune exec -- olox local_reference_self.lox
  (Ast.Class "Foo")
  $ dune exec -- olox reference_self.lox
  (Ast.Class "Foo")
