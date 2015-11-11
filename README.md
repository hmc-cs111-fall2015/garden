[Prolog]: semantics.pl
[LIP]: https://pragprog.com/book/tpdsl/language-implementation-patterns
[Expressions]: https://github.com/hmc-cs111-fall2015/variables-and-functions/releases/tag/Expressions
[Names]: https://github.com/hmc-cs111-fall2015/variables-and-functions/releases/tag/Names
[Statements1]: https://github.com/hmc-cs111-fall2015/variables-and-functions/releases/tag/Statements1
[Statements2]: https://github.com/hmc-cs111-fall2015/variables-and-functions/releases/tag/Statements2
[Variables]: https://github.com/hmc-cs111-fall2015/variables-and-functions/releases/tag/Variables
[Functions1]: https://github.com/hmc-cs111-fall2015/variables-and-functions/releases/tag/Functions1
[Functions2]: https://github.com/hmc-cs111-fall2015/variables-and-functions/releases/tag/Functions2
[Functions3]: https://github.com/hmc-cs111-fall2015/variables-and-functions/releases/tag/Functions3
[Dynamic Scopes]: http://en.wikipedia.org/wiki/Scope_%28computer_science%29#Dynamic_scoping
[Static Scopes]: http://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scoping

# Garden
Garden is a small, garden-variety language to demonstrate how to implement 
variables and functions. (Note, this is only *one* way of implementing these 
language features. Another good resource is the book 
[*Language Implementation Patterns*][LIP]).

## Syntax
Garden supports expressions, printing, sequencing, conditionals, variable 
declarations, assignment, function definitions, and function calls.

The abstract syntax for Garden is:

```
         n ∈ ℤ  x,f ∈ Name

s ∈ Stmt ::= print e | s ; s
          |  if0 (e) then {s} else {s}
          |  var x := e | x := e
          |  def f(x) := {s} | f(e)

e ∈ Expr ::= n | x | e op e | ( e )

op ∈ Operator ::= + | - | * | /
```

## Semantics and Prolog Implementation

The language includes a formal semantics, specified as comments in a 
[Prolog file][Prolog]. The file also includes a Prolog implementation of the
semantics.

## Scala Implementation

The bulk of this repository contains a Scala implementation of Garden. You can
start a read-eval-print loop for Garden by executing
```
sbt run
```
from the top-level directory (i.e., the one that contains this file).

You can run the included test suite by executing
```
sbt test
```
from the top-level directory (i.e., the one that contains this file).

The Scala implementation adds some syntactic sugar to the language:

   - It supports blocks of statements (i.e., sequencing of an arbitrary number
   of statements).
   - It supports functions with multiple parameters

## Growing a Garden

The repository has several tags, which correspond to different versions of
Garden, as it grows from the expression language to its full syntax. The tags
are

  - [`Expressions`][Expressions] : numbers and arithmetic operations
  - [`Statements1`][Statements1] : adds `print` and support for statement sequencing (blocks)
  - [`Statements2`][Statements2] : adds conditionals
  - [`Names`][Names] : adds named constants
  - [`Variables`][Variables] : adds variables, i.e., mutable state
  - [`Functions1`][Functions1] : adds function definitions and function calls, but the implementation doesn't provide scopes
  - [`Functions2`][Functions2] : adds support for scopes, but they're [dynamic][Dynamic Scopes]
  - [`Functions3`][Functions3] : adds support for [static (lexical) scopes][Static Scopes]
