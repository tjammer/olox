Test scopes, variable declaration and printing

  $ dune exec -- olox scopes.lox
  (String "inner a")
  (String "outer b")
  (String "global c")
  (String "outer a")
  (String "outer b")
  (String "global c")
  (String "global a")
  (String "global b")
  (String "global c")

  $ dune exec -- olox decl_scopes.lox
  (Number 3.)
  (Number 2.)
