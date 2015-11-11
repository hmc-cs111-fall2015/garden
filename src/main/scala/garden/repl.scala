package garden

import scala.tools.nsc.EvalLoop
import garden.parser.GardenParser
import garden.semantics.StmtInterpreter.eval

object Garden extends EvalLoop with App {
  override def prompt = "> "

  loop { line ⇒
    GardenParser(line) match {
      case GardenParser.Success(t, _) ⇒
        try {
          println(eval(t))
        } catch {
          case e:ArithmeticException ⇒ println(e.getMessage())
        }
      case e: GardenParser.NoSuccess  ⇒ println(e)
    }
  }
}
