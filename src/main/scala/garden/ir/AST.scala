package garden.ir

sealed abstract class AST

/** Expressions **/
sealed abstract class Expr extends AST
case class Num(n: Int) extends Expr
case class Var(name: String) extends Expr
case class Plus(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr)  extends Expr
case class Mult(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr)  extends Expr

/** Statements **/
sealed abstract class Stmt extends AST
case class Print(expr: Expr) extends Stmt
case class Block(statements: Seq[Stmt]) extends Stmt
case class If0(condition: Expr, trueBranch: Stmt, falseBranch: Stmt) extends Stmt
case class Set(lValue: Var, rValue: Expr) extends Stmt
case class Update(lValue: Var, rValue: Expr) extends Stmt
case class FuncDef(val name: Var, params: List[Var], body: Stmt) extends Stmt
case class Call(name: Var, args: List[Expr]) extends Stmt
