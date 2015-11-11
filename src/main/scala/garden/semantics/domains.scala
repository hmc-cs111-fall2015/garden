package garden

import scala.language.implicitConversions
import garden.ir.Var
import garden.ir.FuncDef

package object semantics {
  /**
   * Domains
   */
  type Value = Either[Int, FuncDef]  
  type Result = (Environment, Store)
  
  type Address = Int
  type Environment = LookupTable[Var, Address]
  type Store = LookupTable[Address, Value]
  
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

  implicit def funcdefToValue(f: FuncDef): Value = Right(f)
  implicit def valueToFuncdef(v: Value): FuncDef = v.right.get

}