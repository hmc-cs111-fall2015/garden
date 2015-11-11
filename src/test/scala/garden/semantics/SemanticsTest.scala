package garden.semantics

import org.scalatest._

import edu.hmc.langtools._
import garden.ir._
import garden.parser._
import garden.semantics._

class GardenExprSemanticsTests extends FunSpec
    with LangInterpretMatchers[Expr, Value] {

  override val parser =
    (s: String) ⇒ GardenParser.parseAll(GardenParser.expr, s)
  override val interpreter = ExprInterpreter.eval _

  describe("A number") {

    it("should evaluate to an integer") {
      program("1") should compute (1)
      program("10") should compute (10)
      program("121") should compute (121)
      program("-10") should compute (-10)
    }

  }

  describe("Addition") {

    it("can add two numbers") {
      program("1+1") should compute (2)
    }

    it("can be chained (and is left-associative)") {
      program("1 + 2 + 100") should compute (103)
    }

    it("can handle negative numbers") {
      program("1 + -1") should compute (0)
    }

  }

  describe("Subtraction") {

    it("can subtract two numbers") {
      program("1-1") should compute (0)
    }

    it("can be chained (and is left-associative)") {
      program("1 - 2 - 100") should compute (-101)
    }

    it("can handle negative numbers") {
      program("1 - -1") should compute (2)
    }

  }

  describe("Multiplication") {

    it("can multiply two numbers") {
      program("1*1") should compute (1)
    }

    it("can be chained (and is left-associative)") {
      program("1 * 2 * 100") should compute (200)
    }

    it("can handle negative numbers") {
      program("1 * -1") should compute (-1)
    }

    it("has higher precedence than addition") {
      program("1 + 2 * 3") should compute (7)
    }

  }

  describe("Division") {

    it("can divide two numbers") {
      program("1/1") should compute (1)
    }

    it("can be chained (and is left-associative)") {
      program("200 / 2 / 10") should compute (10)
    }

    it("can handle negative numbers") {
      program("10 / -2") should compute (-5)
    }

    it("cannot handle divide by zero") {
      program("1 / 0") should raiseError[ArithmeticException]
    }

    it("has higher precedence than subtraction") {
      program("10 - 4 / 2") should compute (8)
    }

  }

  describe("Parenthetical") {

    it("is fine to surround expressions with parentheses") {
      program("(1)") should compute (1)
    }

    it("affects associativity") {
      program("3 * (2 + 1)") should compute (9)
    }

  }
  
  describe("Variables") {
    it("constants are resolved") {
      program("LtUaE") should compute(42)
    }

    it("unbound variables are errors") {
      program("x") should raiseError[LookupException]
    }
  }
}

class GardenStmtSemanticsTests extends FunSpec
    with LangInterpretMatchers[Stmt, Result] 
    with StoreMatchers[Result, Var, Value] {

  override val parser =
    (s: String) ⇒ GardenParser.parseAll(GardenParser.stmt, s)

  override val interpreter = StmtInterpreter.eval _
  
  override def lookup(x: Var, r: Result): Option[Value] = r match {
    case (ρ, σ) ⇒ (ρ get x) flatMap (σ get _)
  }

  describe("Blocks") {
    it("combine two or more statements, separated by a semicolon") {
      program("print 1+1; print LtUaE") should give ( )
    }
    
    it("combine two or more statements, separated by a semicolon and updates the store") {
      program("var x := 1; var y := x") should give(Var("x") → 1, Var("y") → 1)
    }
  }
  
  describe("If0 statements") {
    it("have a evaluate the true branch if the condition is 0") {
      program("if0 (0) then {print 0} else {print 1}") should give ( )
    }
    
     it("execute the true branch if the condition is true and update the store") {
      program("""var result := -1; 
                 if0 (0) 
                 then { result:=0 } 
                 else { result:=10000 }""") should
        give (Var("result") → 0)
    }

    it("execute the false branch if the condition is false") {
      program("""var result := -1; 
                 if0 (1) 
                 then { result:=0 } 
                 else { result:=10000 }""") should
        give (Var("result") → 10000)
    }

    it("can have complex conditions") {
      program("""var result := -1; 
                 if0 (1-1) 
                 then { result:=0 } 
                 else { result:=10000 }""") should
        give (Var("result") → 0)
    }
  }
  
  describe("Variable definition") {
    it("assigns the result of an expression to a variable") {
      program("var x := 1") should give (Var("x") → 1)
    }
  }

  describe("Variable redefinition") {
    it("assigns the result of an expression to a variable") {
      program("var x := 0; x := 1") should give (Var("x") → 1)
    }
  }

  describe("Function calls") {

    it("evaluate the arguments, bind the results to the parameters and execute the body") {
      program("var result := 0; def double(x) := {result := 2 * x}; double(2)") should
        give (Var("result") → 4)
    }

    it("should have access to global variables") {
      program("var x := 3; var result := 0; def addX(y) := {result := x + y}; addX(2)") should
        give (Var("result") → 5)
    }

    it("should have static scope") {
      program("var result := 0; def addX(y) := {result := x + y}; addX(2); var x := 3 ") should
        raiseError[LookupException]

      program("var result := 0; def f(x) := {g(x+2)}; def g(y) := {result := x + y}; f(2)") should 
        raiseError[LookupException]
    }

    it("should respect local and global scope") {
      program("var x := 1000; var result := 0; def id(x) := {result := x; x := result}; id(1)") should
        give (Var("x") → 1000, Var("result") → 1)
    }

    // note: this test intentionally fails
    it("should allow recursion") {
      program("""var result := 0; 
                 def fact(n) := { 
                    if0 (n) 
                    then {result := 1} 
                    else {fact(n - 1); result := n * result}
                 }; 
                 fact(5)""") should give (Var("result") → 120)
    }
    
    it("should technically allow higher-order functions") {
        program("""var result :=0;
                   var G := 0; 
                   def f(x) := {
                     def g(y) := {result := x+y}; 
                     G:=g
                   }; 
                   f(2); 
                   G(3)""") should give (Var("result") → 5)
    }

  }
}
