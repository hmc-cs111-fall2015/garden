package garden.parser

import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.PackratParsers
import garden.ir.Expr
import garden.ir.ExprBuilder
import garden.ir.Num
import garden.ir.Print
import garden.ir.Stmt
import garden.ir.Block

/**
 * The parser accepts the following language

                n ∈ ℤ

        s ∈ Stmt ::= `print` e | s `;` s
        
        e ∈ Expr ::= n | e op e | `(` e `)`
        
        op ∈ Operator ::= `+` | `-` | `*` | `/`

 * The parser ignores whitespace. 
 * ℤ is parsed using JavaTokenParsers' wholeNumber parser.
 */
object GardenParser extends JavaTokenParsers with PackratParsers {

    // parsing interface
    def apply(s: String): ParseResult[Stmt] = parseAll(stmt, s)

    /** statements **/
    lazy val stmt: PackratParser[Stmt] = 
      (   rep1sep(stmt, ";") ^^ Block 
        | "print"~>expr ^^ Print
        | failure("expected a statement"))
        
    /** expressions **/
    lazy val expr: PackratParser[Expr] = 
      (   expr~"+"~term ^^ {case l~"+"~r ⇒ l |+| r}
        | expr~"-"~term ^^ {case l~"-"~r ⇒ l |-| r}
        | term )

    // terms
    lazy val term: PackratParser[Expr] = 
      (  term~"*"~fact ^^ {case l~"*"~r ⇒ l |*| r}
       | term~"/"~fact ^^ {case l~"/"~r ⇒ l |/| r}
       | fact )
        
    // factors
    lazy val fact: PackratParser[Expr] =
      (   number
        | "("~>expr<~")" 
        | failure("expected an expression"))
        
    // numbers
    def number: Parser[Num] = wholeNumber ^^ {s ⇒ Num(s.toInt)}
 }
