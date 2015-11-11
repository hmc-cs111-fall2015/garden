package garden

import scala.language.implicitConversions
import garden.ir.Var
import garden.ir.FuncDef

package object semantics {
  /**
   * Domains
   */
  type Value = Either[Int, FuncDef]  
  type Result = Store
  
  type Store = LookupTable[Var, Value]
  
   /**
   * Initial values
   */
  val σ0: Store = LookupTable.empty[Var, Value] + (Var("LtUaE") → 42)
  
  /**
   * Implicit conversions
   */
  implicit def intToValue(i: Int): Value = Left(i)
  implicit def valueToInt(v: Value): Int = v.left.get

  implicit def funcdefToValue(f: FuncDef): Value = Right(f)
  implicit def valueToFuncdef(v: Value): FuncDef = v.right.get

}