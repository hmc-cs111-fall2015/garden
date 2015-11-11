package garden.semantics

import garden.ir._

/**
 * ExprInterpreter can evaluate an Expr
 */
object ExprInterpreter {
  /** evaluating an expression **/
  def eval(expr: Expr): Value = expr match {
    case Num(i)            ⇒ i
    case Plus(left, right) ⇒ eval(left) + eval(right)
    case Sub(left, right)  ⇒ eval(left) - eval(right)
    case Mult(left, right) ⇒ eval(left) * eval(right)
    case Div(left, right)  ⇒ eval(left) / eval(right)
  }
}
