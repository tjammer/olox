We can declare a class

  $ dune exec -- olox decl.lox
  (Class "DevonshireCream")
  (Instance "DevonshireCream")
  RuntimeError: Undefined property on DevonshireCream: field

We can access and define fields

  $ dune exec -- olox fields.lox
  (String "too sweet")
  (Number 12.)
  (Number 42.)
  RuntimeError: Assignment target with get expression must be instance, not: (Class "Bagel")

Call methods

  $ dune exec -- olox methods.lox
  (String "Crunch crunch crunch!")
  (String "Crunch crunch crunch!")

Bound methods and 'this'

  $ dune exec -- olox this_bound_methods.lox
  (String "Jane")
  (Instance "Egotist")
  (String "The German chocolate cake is delicious!")

'This' outside class is not allowed

  $ dune exec -- olox outside_class_this.lox
  StaticError: Can't use 'this' outside of a class

Initializers

  $ dune exec -- olox init.lox
  (String "init")
  (String "directly")
  (Instance "Foo")
  (Number 10.)
  (Instance "Foo")
  (Instance "ReturnInInit")

Return with expression in initializer

  $ dune exec -- olox return_from_init.lox
  StaticError: Can't return a value from an initializer

Tests from reference repo

  $ dune exec -- olox empty.lox
  (Class "Foo")
  $ dune exec -- olox inherit_self.lox
  StaticError: A class can't inherit from itself: Foo
  $ dune exec -- olox inherited_method.lox
  (String "in foo")
  (String "in bar")
  (String "in baz")
  $ dune exec -- olox local_inherit_other.lox
  (Class "B")
  $ dune exec -- olox local_inherit_self.lox
  StaticError: A class can't inherit from itself: Foo
  $ dune exec -- olox local_reference_self.lox
  (Class "Foo")
  $ dune exec -- olox reference_self.lox
  (Class "Foo")
