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

/**
 * StmtInterpreter can evaluate a Stmt
 */

object StmtInterpreter {
  import ExprInterpreter.{eval ⇒ evalE}
  
  /** evaluating a statement **/
  def eval(stmt: Stmt): Result = stmt match {
    case Print(e)         ⇒ evalPrint(e)
    case Block(stmts)     ⇒ evalBlock(stmts)
  }

  /** print **/
  def evalPrint(expr: Expr): Result = {
    // (1) evaluate the expression
    val v = evalE(expr)

    // (2) print the result
    println(v)
  }
  
  /** blocks **/
  def evalBlock(stmts: Seq[Stmt]): Result = 
    stmts foreach eval
}