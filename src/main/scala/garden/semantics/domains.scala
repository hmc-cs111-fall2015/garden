package garden

import scala.language.implicitConversions
import garden.ir.Var

package object semantics {
  /**
   * Domains
   */
  type Value = Int
  type Result = Unit
  
  type Store = LookupTable[Var, Value]
  
   /**
   * Initial values
   */
  val σ0: Store = LookupTable.empty[Var, Value] + (Var("LtUaE") → 42)
}