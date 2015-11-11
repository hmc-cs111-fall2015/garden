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
import garden.ir.If0
import garden.ir.Var

/**
 * The parser accepts the following language

                n ∈ ℤ     x ∈ Name

        s ∈ Stmt ::= `print` e | s `;` s
                  |  `if0` `(` e `)` `then` `{` s `}` `else` `{` s `}`      
        
        e ∈ Expr ::= n | x | e op e | `(` e `)`
        
        op ∈ Operator ::= `+` | `-` | `*` | `/`

 * The parser ignores whitespace. 
 * ℤ is parsed using JavaTokenParsers' wholeNumber parser.
 * Name is parsed using JavaTokenParsers' ident parser.
 */
object GardenParser extends JavaTokenParsers with PackratParsers {

    // parsing interface
    def apply(s: String): ParseResult[Stmt] = parseAll(stmt, s)

    /** statements **/
    lazy val stmt: PackratParser[Stmt] = 
      (   rep1sep(stmt, ";") ^^ Block 
        | "print"~>expr ^^ Print
        | ifStmt
        | failure("expected a statement"))
        
    // if stmts
    def ifStmt: Parser[If0] =
      "if0"~"("~expr~")"~"then"~"{"~stmt~"}"~"else"~"{"~stmt~"}" ^^
         { case "if0"~"("~c~")"~"then"~"{"~t~"}"~"else"~"{"~f~"}" ⇒ If0(c,t,f) }

        
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
        | variable
        | "("~>expr<~")" 
        | failure("expected an expression"))
        
    // variables
    def variable: Parser[Var] = ident ^^ Var

    // numbers
    def number: Parser[Num] = wholeNumber ^^ {s ⇒ Num(s.toInt)}
 }
