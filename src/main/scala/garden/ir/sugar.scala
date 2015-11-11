package garden

import scala.language.implicitConversions

// internal DSL for creating ASTs
package object ir {

  // to use a number on its own
  implicit def int2Number(i: Int): Num = Num(i)
  
  // to use a number as part of a binary operation...
  implicit def intToExprBuilder(n: Int) = new ExprBuilder(Num(n))

  // to build up operations using infix notation from left to right...
  // ExprBuilder saves the left operand and defines methods that 
  //   take the right operand and returns the appropriate Expr 
  implicit class ExprBuilder(val left: Expr) {
    def |+|(right: Expr) = Plus(left, right)
    def |-|(right: Expr) = Sub(left, right)
    def |*|(right: Expr) = Mult(left, right)
    def |/|(right: Expr) = Div(left, right)
  }
}
