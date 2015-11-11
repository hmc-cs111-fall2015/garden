package garden.semantics

import garden.ir._

/**
 * ExprInterpreter can evaluate an Expr
 */
object ExprInterpreter {
  /** evaluating an expression **/
  def eval(expr: Expr): Value = evalE(expr, ρ0, σ0)

  def evalE(expr: Expr, ρ: Environment, σ: Store): Value = expr match {
    case Num(i)            ⇒ i
    case x: Var            ⇒ σ(ρ(x))
    case Plus(left, right) ⇒ evalE(left, ρ, σ) + evalE(right, ρ, σ)
    case Sub(left, right)  ⇒ evalE(left, ρ, σ) - evalE(right, ρ, σ)
    case Mult(left, right) ⇒ evalE(left, ρ, σ) * evalE(right, ρ, σ)
    case Div(left, right)  ⇒ evalE(left, ρ, σ) / evalE(right, ρ, σ)
  }
}

/**
 * StmtInterpreter can evaluate a Stmt
 */

object StmtInterpreter {
  import ExprInterpreter.evalE
  
  /** addresses and allocation **/
  private var nextAddress: Address = 0
  def alloc(): Address = {
    nextAddress += 1; 
    nextAddress
  }

  /** evaluating a statement **/
  def eval(stmt: Stmt): Result = evalS(stmt, ρ0, σ0)

  def evalS(stmt: Stmt, ρ: Environment, σ: Store): Result = stmt match {
    case Print(e)         ⇒ evalPrint(e, ρ, σ)
    case Block(stmts)     ⇒ evalBlock(stmts, ρ, σ)
    case If0(e, s_t, s_f) ⇒ evalIf0(e, s_t, s_f, ρ, σ)
    case Set(x, e)        ⇒ evalAssign(x, e, ρ, σ)
    case Update(x, e)     ⇒ evalUpdate(x, e, ρ, σ)    
    case f: FuncDef       ⇒ evalFuncDef(f, ρ, σ)   
    case Call(f, args)    ⇒ evalCall(f, args, ρ, σ)    
  }

  /** print **/
  def evalPrint(expr: Expr, ρ: Environment, σ: Store): Result = {
    // (1) evaluate the expression
    val v = evalE(expr, ρ, σ)

    // (2) print the result
    println(v)
    
    (ρ, σ) // printing doesn't affect the result
  }
  
  /** blocks **/
  def evalBlock(stmts: Seq[Stmt], ρ: Environment, σ: Store): Result = 
    if (stmts.isEmpty)
      (ρ, σ)  // Empty blocks don't affect the result
    else {
      // (1) evaluate 1st statement
      val (ρ1, σ1) = evalS(stmts.head, ρ, σ)  

      // (2) use result to evaluate rest of block
      evalS(Block(stmts.tail), ρ1, σ1)  
    }
  /**
   * Note: we could also have written this code using a left-fold:
   *       
      def doNext(state: (Environment, Store), stmt: Stmt): Result = 
        evalS(stmt, state._1, state._2)
        
      ( (ρ, σ) /: stmts )(doNext)
   * 
   */
    
  /** if0 **/
  def evalIf0(condition: Expr, trueBranch: Stmt, falseBranch: Stmt,
              ρ: Environment, σ: Store) = {
    // (1) evaluate condition
    // (1) evaluate condition
    val conditionValue = evalE(condition, ρ, σ)
    
    // error checking: condition must be a number
    require(conditionValue.isLeft, "condition must be a number")

    // (2) based on condition's value, evaluate true or false branch
    if (conditionValue.left.get == 0)
      evalS(trueBranch, ρ, σ)
    else 
      evalS(falseBranch, ρ, σ)
  }
  
  /** variable creation **/
  def evalAssign(variable: Var, expr: Expr, ρ: Environment, σ: Store): Result = {
    // error checking: make sure that var is not defined
    require(!(ρ contains variable), 
            s"Redefinition of variable ${variable.name}")
    
    // (1) Evaluate the right-hand side 
    val value = evalE(expr, ρ, σ)
    
    // (2) Allocate space for the variable
    val a = alloc()
    
    // (3) Bind the variable to the address in the environment
    val ρ1 = ρ + (variable → a)

    // (4) Bind the address to the value in the store
    val σ1 = σ + (a → value)
    
    (ρ1, σ1)
  }
  
  /** variable update **/
  def evalUpdate(variable: Var, expr: Expr, ρ: Environment, σ: Store): Result = {
    // error checking: make sure that var is defined
    require(ρ contains variable, 
            s"""Cannot update non-existent variable ${variable.name}. 
    Try var ${variable.name} := ... ?""")
            
    // (1) Evaluate the right-hand side 
    val value = evalE(expr, ρ, σ)
    
    // (2) Lookup the variable's address 
    val a = ρ(variable)
    
    // (3) Bind the address to the value in the store
    val σ1 = σ + (a → value)
    
    (ρ, σ1)
  }
  
  /** function definitions **/
  def evalFuncDef(f: FuncDef, ρ: Environment, σ: Store): Result = {
     // (1) Allocate space for the function
    val a: Address = alloc()
    
    // (2) Bind the function name to the new address in the environment
    val ρ1: Environment = ρ + (f.name → a)

    // (3) Make a closure for the function 
    val closure: Value = Closure(f, ρ)   

    // (4) Bind the new address to the closure in the store
    val σ1 = σ + (a → closure)

    (ρ1, σ1)
  }
  
  def evalCall(f: Var, args: List[Expr], ρ: Environment, σ: Store): Result = {

    // (1) Lookup the function, to get its value
    val func: Value = evalE(f, ρ, σ)

    // type checking: require f to be a function
    require(func.isRight, s"cannot call ${f.name}: it is not a function")

    val Closure(FuncDef(name, params, body), ρC) = func.right.get

    // error checking: require |args| = |params|
    val argsLength = args.length
    val paramsLength = params.length
    require(args.length == params.length,
      s"${f.name} expects $paramsLength arg(s) but got $argsLength")

    // (2) evaluate the arguments
    val argValues = args map (evalE(_, ρ, σ))

    // (3) allocate space for each parameter
    val addrs: Seq[Address] = params map (_ ⇒ alloc())
    
    // (4) bind each parameter name to its address in the closure's environment
    val ρC1: Environment = ρC ++ (params zip addrs)
    
    // (5) bind each parameter address to its corresponding value
    val σ1: Store = σ ++ (addrs zip argValues)

    // (6) evaluate the body with the new environment and store
    val (ρ2, σ2) = evalS(body, ρC1, σ1)

    /** WARNING: here's a naïve way to do garbage collection, but it doesn't 
     *  work because closures can capture addresses that live longer than this 
     *  function call:
     *  
         val σ3 = σ2 -- addrs
     *  
     */
    
    (ρ, σ2)     // Function calls don't affect the *current* environment
  }
}