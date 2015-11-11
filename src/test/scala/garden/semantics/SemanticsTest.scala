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
}
