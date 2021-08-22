We can declare a class

  $ dune exec -- olox decl.lox
  (Ast.Class "DevonshireCream")
  RuntimeError: Undefined property on DevonshireCream: field

  $ dune exec -- olox fields.lox
  (Ast.String "too sweet")
  (Ast.Number 12.)
  (Ast.Number 42.)
  RuntimeError: Assignment target with get expression must be instance, not: (Ast.Class "Bagel")
