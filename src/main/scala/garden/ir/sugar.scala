package garden

import scala.language.implicitConversions

// internal DSL for creating ASTs
package object ir {

  // to use a number on its own
  implicit def int2Number(i: Int): Num = Num(i)
  
  // to use a number as part of a binary operation...
  implicit def intToExprBuilder(n: Int) = new ExprBuilder(Num(n))

  // to use a variable on its own
  implicit def symbol2Var(s: Symbol): Var = Var(s.toString.tail)
  
  // to use a variable as part of a binary operation...
  implicit def symbolToExprBuilder(s: Symbol) = 
    new ExprBuilder(Var(s.toString.tail))

  // to build up operations using infix notation from left to right...
  // ExprBuilder saves the left operand and defines methods that 
  //   take the right operand and returns the appropriate Expr 
  implicit class ExprBuilder(val left: Expr) {
    def |+|(right: Expr) = Plus(left, right)
    def |-|(right: Expr) = Sub(left, right)
    def |*|(right: Expr) = Mult(left, right)
    def |/|(right: Expr) = Div(left, right)
  }
  
  // to use a variable as part of an assignment
  implicit def symbolToStmtBuilder(s: Symbol) = 
    new StmtBuilder(Var(s.toString.tail))

  // to build up statements using infix notation from left to right...
  implicit class StmtBuilder(val left: Var) {
    def |←|(right: Expr) = Set(left, right)
    def |:=|(right: Expr) = Update(left, right)
  }
}
