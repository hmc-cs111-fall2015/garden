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
    case If0(e, s_t, s_f) ⇒ evalIf0(e, s_t, s_f)
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
    
  /** if0 **/
  def evalIf0(condition: Expr, trueBranch: Stmt, falseBranch: Stmt) = {
    // (1) evaluate condition
    val conditionValue = evalE(condition)
    
    // (2) based on condition's value, evaluate true or false branch
    if (conditionValue == 0)
      eval(trueBranch)
    else 
      eval(falseBranch)
  }
}