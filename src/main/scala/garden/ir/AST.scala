package garden.ir

sealed abstract class AST

/** Expressions **/
sealed abstract class Expr extends AST
case class Num(n: Int) extends Expr
case class Plus(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr)  extends Expr
case class Mult(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr)  extends Expr

/** Statements **/
sealed abstract class Stmt extends AST
case class Print(expr: Expr) extends Stmt
case class Block(statements: Seq[Stmt]) extends Stmt
