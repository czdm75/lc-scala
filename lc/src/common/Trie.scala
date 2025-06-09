package common

import scala.annotation.tailrec
import common.Zero

trait TrieTree[Self <: TrieTree[Self]] {
  self: Self =>

  // public APIs
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def contains(str: String): Boolean = this.search(str.toList, prefix = false)
  def containsPrefix(str: String): Boolean = this.search(str.toList, prefix = true)

  // internal APIs
  def isTerminate: Boolean
  def getSubTree(c: Char): Option[Self]
  def containsSubTree(c: Char): Boolean

  @tailrec
  private def search(s: List[Char], prefix: Boolean): Boolean = s match {
    case Nil                             => prefix || isTerminate
    case hd :: tl if containsSubTree(hd) => search(tl, prefix)
    case _                               => false
  }
}

trait ImmutableTrieTree[Self <: ImmutableTrieTree[Self]] extends TrieTree[Self] {
  self: Self =>

  // public APIs
  def +(str: String): Self
  def -(str: String): Self

  // internal APIs
  def terminated: Self
  def unterminated: Self
  def updatedSubTree(c: Char)(f: Option[Self] => Option[Self]): Self
}

trait MutableTrieTree[Self <: MutableTrieTree[Self]] extends TrieTree[Self] {
  self: Self =>

  // public APIs
  def +=(str: String): Self
  def -=(str: String): Self

  // internal APIs
  def terminate: Self
  def unterminate: Self
  def updateSubTree(c: Char)(f: Option[Self] => Option[Self]): Self
}

// implementation of map-based read only immutable trie tree
abstract class MapBasedTrieTree[Self <: MapBasedTrieTree[Self]](
    map: scala.collection.Map[Char, Self],
    isTerminate: Boolean
) extends TrieTree[Self] {
  self: Self =>
  def isEmpty = map.isEmpty && !isTerminate
  def nonEmpty = map.nonEmpty || isTerminate
  def apply(c: Char): Self = map(c)
  def getSubTree(c: Char): Option[Self] = map.get(c)
  def containsSubTree(c: Char): Boolean = map contains c
}
