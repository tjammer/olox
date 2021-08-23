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
