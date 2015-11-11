package garden.semantics

import java.util.NoSuchElementException

import scala.collection.MapLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.mutable.MapBuilder

/**
 * Custom exception for when lookup fails
 */
class LookupException(msg: String) extends RuntimeException(msg)

/**
 * A lookup table that has the same interface as a map *and* provides support
 * for a stack-based implementation of scopes (similar to Chapter 6 of 
 * "Language Implementation Patterns") 
 * 
 * Design inspired by "The Architecture of Scala Collections"
 * docs.scala-lang.org/overviews/core/architecture-of-scala-collections.html
 */
class LookupTable[K, +V] private (val scopes: List[Map[K, V]] = List())
    extends Map[K, V] with MapLike[K, V, LookupTable[K, V]] {

  /** the Map and MapLike interfaces */
  override def empty = new LookupTable[K, V].pushScope

  override def get(key: K): Option[V] =
    scopes.flatten.collectFirst({case (`key`, value) ⇒ value})

  override def iterator = scopes.flatten.iterator

  override def +[B1 >: V](kv: (K, B1)) =
    new LookupTable(scopes.updated(0, scopes.head + kv))

  override def -(key: K) = {
    // remove the binding only from the first binding in the chain 
    // there's probably a more elegant way to do this

    // split at first def
    val (defed, undefed) = scopes span (_.contains(key))

    // unbind the def, if we found it 
    val newDefed = defed match {
      case Nil          ⇒ Nil
      case head :: rest ⇒ (head - key) :: rest
    }

    // construct a new lookup table
    new LookupTable(undefed ::: newDefed)
  }
  
  // convert failed lookups into LookupExceptions
  override def apply(key: K): V =
    try {
      super.apply(key)
    } catch {
      case e: NoSuchElementException ⇒ 
        throw new LookupException(s"$key is undefined")
    }

  // Additional functionality for a stack-based implementation of scopes
  def pushScope(): LookupTable[K, V] = new LookupTable(newScope :: scopes)
  def popScope():  LookupTable[K, V] = new LookupTable(scopes.tail)
  def isInScope(key: K) = scopes.head.contains(key)

  private def newScope = Map.empty[K, V]
}

object LookupTable {
  def empty[K, V] = new LookupTable[K, V].pushScope
  def apply[K, V](kvs: (K, V)*): LookupTable[K, V] = (empty[K, V] /: kvs)(_ + _)

  def newBuilder[K, V]: Builder[(K, V), LookupTable[K, V]] =
    new MapBuilder[K, V, LookupTable[K, V]](empty[K,V])

  implicit def canBuildFrom[K, V]
    : CanBuildFrom[LookupTable[_,_], (K, V), LookupTable[K, V]] = 
      new CanBuildFrom[LookupTable[_, _], (K, V), LookupTable[K, V]] {
        def apply(from: LookupTable[_, _]) = newBuilder[K, V]
        def apply() = newBuilder[K, V]
      }  
}
