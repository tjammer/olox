# Olox
Olox is a tree-walk interpreter for the Lox language after the first half of [Crafting Interpreters](http://craftinginterpreters.com/), implemented in OCaml (hence the name).

## Implementation
There are some differences to the original book implementation in Java. First of all, instead of writing everything from scratch, olox uses ocamllex and [menhir](http://gallium.inria.fr/~fpottier/menhir/) for lexing and parsing, respectively. The reason for this is that I have written recursive descent parser before but had never used a parser generator.

Instead of using classes and the visitor pattern for AST traversal, Olox encodes the AST in variants (ADTs), which is the natural solution in OCaml (even though the "o" in olox comes from OCaml's "Objective"). However, the resulting structure of the code is similar enough to the book's solution.

For the environments, olox uses a persistent data structure (`Map.t` of the OCaml standard library). Thus, a "resolver" is not strictly needed for lexical scopes. Nevertheless, there is a static analysis pass which runs before the interpreter. Like the resolver of jlox, it makes sure that all variables can be resolved and throws the some static errors (usage of `this` outside of method, `super` in a class without a superclass, etc.).

## Jlox compliance
For the most part, olox behaves exactly like jlox. In particular, olox passes the test suites for "functions", "closures" and "classes" from [the original repo](https://github.com/munificent/craftinginterpreters), see test directory.

There are two exceptions:

### 1. Mutual recursion
In olox, the global environment is simply the first environment, but not special otherwise. As a result, variables in the global environment cannot be resolved before they are declared. This breaks naive mutual recursion, because the first function won't be able to see the second function. To use mutual recursion, the user can pass the second function into the first one as a parameter.

### 2. Parameter limit
I did not implement an upper limit of 255 parameters / arguments. Predictably, these tests cannot pass.

## Building and running
Olox uses the [dune](https://dune.build/) build system, so the usual dune commands work

``` ocaml
dune build
```

Similarly, to run the tests

``` ocaml
dune runtest
```

