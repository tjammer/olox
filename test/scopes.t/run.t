Test scopes, variable declaration and printing

  $ dune exec -- olox scopes.lox
  (Ast.String "inner a")
  (Ast.String "outer b")
  (Ast.String "global c")
  (Ast.String "outer a")
  (Ast.String "outer b")
  (Ast.String "global c")
  (Ast.String "global a")
  (Ast.String "global b")
  (Ast.String "global c")

  $ dune exec -- olox decl_scopes.lox
  (Ast.Number 3.)
  (Ast.Number 2.)
