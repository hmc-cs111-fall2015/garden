package garden.parser

import org.scalatest._

import garden.ir._
import edu.hmc.langtools._

class GardenExprParserTests extends FunSpec with LangParseMatchers[AST] {

  override val parser = 
    (s: String) ⇒ GardenParser.parseAll(GardenParser.expr, s)
  
  describe("A number") {

    it("can be a single digit") {
      program("1") should parseAs ( 1 )
    }
    
    it ("can be multiple digits") {
      program("10") should parseAs ( 10 )
      program("121") should parseAs ( 121 )
    }
    
    it ("can be a negative number") {
      program("-10") should parseAs ( -10 )
    }
    
    it ("cannot be floating-point number") {
      program("1.1") should not (parse)
      program(" .3") should not (parse)
    }

  }
  
  describe("Addition") {

    it("can add two numbers") {
      program("1+1") should parseAs ( 1 |+| 1 )
    }
    
    it("can be chained (and is left-associative)") {
      program("1 + 2 + 100") should parseAs ( (1 |+| 2) |+| 100 )
    }

  }
  
  describe("Subtraction") {

    it("can subtract two numbers") {
      program("1-1") should parseAs ( 1 |-| 1 )
    }
    
    it("can be chained (and is left-associative)") {
      program("1 - 2 - 100") should parseAs ( (1 |-| 2) |-| 100 )
    }

  }
  
  describe("Multiplication") {

    it("can multiply two numbers") {
      program("1*1") should parseAs ( 1 |*| 1 )
    }
    
    it("can be chained (and is left-associative)") {
      program("1 * 2 * 100") should parseAs ( (1 |*| 2) |*| 100 )
    }

  }
  
  describe("Division") {

    it("can divide two numbers") {
      program("1/1") should parseAs ( 1 |/| 1 )
    }
    
    it("can be chained (and is left-associative)") {
      program("1 / 2 / 100") should parseAs ( (1 |/| 2) |/| 100 )
    }

  }
  
  describe("Parenthetical") {

    it("is fine to surround expressions with parentheses") {
      program("(1)") should parseAs ( 1 )
    }
    
    it("affects associativity") {
      program("1 * (2 + 3)") should parseAs ( 1 |*| (2 |+| 3) )
    }

  }
  
  describe("Variables") {

    it("are valid expressions") {
      program("x") should parseAs ( 'x )
    }

  }

  
}

class GardenStmtParserTests extends FunSpec with LangParseMatchers[AST] {

  override val parser = 
    (s: String) ⇒ GardenParser.parseAll(GardenParser.stmt, s)
    
  describe("Print statements") {
    it("can print a simple expression") {
      program("print 1") should parseAs (Print(1))
    }
    
    it("can print a complex expression") {
      program("print 1+1") should parseAs (Print(1 |+| 1))
    }
  }
  
  describe("Blocks") {
    it("combine two or more statements, separated by a semicolon") {
      program("print 1+1; print 3") should parseAs (Block(List(Print(1 |+| 1), Print(3))))
    }
  }
  
  describe("If0 statements") {
    it("have a simple condition, true branch, and false branch") {
      program("if0 (0) then {print 0} else {print 1}") should 
        parseAs (If0(0, Print(0), Print(1)))
    }

    it("have a comples condition, true branch, and false branch") {
      var trueBlock = Block(List(Print(0), Print(0)))
      var falseBlock = Block(List(Print(1), Print(1)))
      program("if0 (1-1) then {print 0; print 0} else {print 1; print 1}") should 
        parseAs (If0(1|-|1, trueBlock, falseBlock))
    }
  }
  
}
