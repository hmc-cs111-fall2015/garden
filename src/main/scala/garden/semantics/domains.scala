package garden

import scala.language.implicitConversions
import garden.ir.Var
import garden.ir.FuncDef

package object semantics {
  /**
   * Domains
   */
  type Value = Either[Int, Closure]  
  type Result = (Environment, Store)
  
  type Address = Int
  type Environment = LookupTable[Var, Address]
  type Store = LookupTable[Address, Value]
  case class Closure(f: FuncDef, ρ: Environment)  
  
   /**
   * Initial values
   */
  val ρ0: Environment = LookupTable.empty[Var, Address] + (Var("LtUaE") → 0)
  val σ0: Store = LookupTable.empty[Address, Value] + (0 → 42)
  
  /**
   * Implicit conversions
   */
  implicit def intToValue(i: Int): Value = Left(i)
  implicit def valueToInt(v: Value): Int = v.left.get

  implicit def closureToValue(c: Closure): Value = Right(c)
  implicit def valueToClosure(v: Value): Closure = v.right.get
}
