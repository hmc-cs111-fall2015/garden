package garden.semantics

import garden.ir._

/**
 * ExprInterpreter can evaluate an Expr
 */
object ExprInterpreter {
  /** evaluating an expression **/
  def eval(expr: Expr): Value = evalE(expr, σ0)

  def evalE(expr: Expr, σ: Store): Value = expr match {
    case Num(i)            ⇒ i
    case x: Var            ⇒ σ(x)
    case Plus(left, right) ⇒ evalE(left, σ) + evalE(right, σ)
    case Sub(left, right)  ⇒ evalE(left, σ) - evalE(right, σ)
    case Mult(left, right) ⇒ evalE(left, σ) * evalE(right, σ)
    case Div(left, right)  ⇒ evalE(left, σ) / evalE(right, σ)
  }
}

/**
 * StmtInterpreter can evaluate a Stmt
 */

object StmtInterpreter {
  import ExprInterpreter.evalE
  
  /** evaluating a statement **/
  def eval(stmt: Stmt): Result = evalS(stmt, σ0)

  def evalS(stmt: Stmt, σ: Store): Result = stmt match {
    case Print(e)         ⇒ evalPrint(e, σ)
    case Block(stmts)     ⇒ evalBlock(stmts, σ)
    case If0(e, s_t, s_f) ⇒ evalIf0(e, s_t, s_f, σ)
    case Set(x, e)        ⇒ evalAssign(x, e, σ)
    case Update(x, e)     ⇒ evalUpdate(x, e, σ)    
    case f: FuncDef       ⇒ evalFuncDef(f, σ)   
    case Call(f, args)    ⇒ evalCall(f, args, σ)    
  }

  /** print **/
  def evalPrint(expr: Expr, σ: Store): Result = {
    // (1) evaluate the expression
    val v = evalE(expr, σ)

    // (2) print the result
    println(v)
    
    σ // printing doesn't affect the result
  }
  
  /** blocks **/
  def evalBlock(stmts: Seq[Stmt], σ: Store): Result = 
    if (stmts.isEmpty)
      σ  // Empty blocks don't affect the result
    else {
      // (1) evaluate 1st statement
      val σ1 = evalS(stmts.head, σ)  

      // (2) use result to evaluate rest of block
      evalS(Block(stmts.tail), σ1)  
    }
  /**
   * Note: we could also have written this code using a left-fold:
   *       
      def doNext(store: Store, stmt: Stmt): Result = 
        evalS(stmt, store)
        
      ( σ /: stmts )(doNext)
   * 
   */
    
  /** if0 **/
  def evalIf0(condition: Expr, trueBranch: Stmt, falseBranch: Stmt,
              σ: Store) = {
    // (1) evaluate condition
    val conditionValue = evalE(condition, σ)
    
    // error checking: condition must be a number
    require(conditionValue.isLeft, "condition must be a number")
    
    // (2) based on condition's value, evaluate true or false branch
    if (conditionValue.left.get == 0)
      evalS(trueBranch, σ)
    else 
      evalS(falseBranch, σ)
  }
  
    /** variable creation **/
  def evalAssign(variable: Var, expr: Expr, σ: Store): Result = {
    // error checking: make sure that var is not defined
    require(!(σ contains variable), 
            s"Redefinition of variable ${variable.name}")
    
    // (1) Evaluate the right-hand side 
    val value = evalE(expr, σ)
    
    // (2) Bind the name to the value in the store
    val σ1 = σ + (variable → value)
    
    σ1
  }
  
  /** variable update **/
  def evalUpdate(variable: Var, expr: Expr, σ: Store): Result = {
    // error checking: make sure that var is defined
    require(σ contains variable, 
            s"""Cannot update non-existent variable ${variable.name}. 
    Try var ${variable.name} := ... ?""")
            
    // (1) Evaluate the right-hand side 
    val value = evalE(expr, σ)
    
    // (3) Bind the name to the new value in the store
    val σ1 = σ + (variable → value)
    
    σ1
  }
  
  /** function definitions **/
  def evalFuncDef(f: FuncDef, σ: Store): Result = {
    // (1) Bind the function name to its value in the store
    val σ1: Store = σ + (f.name → f)

    σ1
  }
  
  def evalCall(f: Var, args: List[Expr], σ: Store) = {

    // (1) Lookup the function, to get its value
    val func: Value = evalE(f, σ)

    // type checking: require f to be a function
    require(func.isRight, s"cannot call ${f.name}: it is not a function")

    val FuncDef(name, params, body) = func.right.get

    // error checking: require |args| = |params|
    val argsLength = args.length
    val paramsLength = params.length
    require(args.length == params.length,
      s"${f.name} expects $paramsLength arg(s) but got $argsLength")

    // (2) evaluate the arguments
    val argValues = args map (evalE(_, σ))

    // (3) bind each parameter name to its value in the store
    val σ1: Store = σ ++ (params zip argValues)

    // (4) evaluate the body with the new store
    val σ2 = evalS(body, σ1)

    σ2
  }
}