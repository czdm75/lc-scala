package common.mutable

import scala.collection.mutable
import scala.annotation.tailrec

import common.{Zero, FluentMutable, TrieTree, MutableTrieTree, MapBasedTrieTree}

trait RecursiveTrieTree[Self <: RecursiveTrieTree[Self]](using z: Zero[Self]) extends MutableTrieTree[Self] {
  self: Self =>
  def +=(str: String): Self = self += str.toList
  def -=(str: String): Self = self -= str.toList

  protected def +=(s: List[Char]): Self = s match {
    case Nil      => self.terminate
    case hd :: tl => self.updateSubTree(hd) { trieOpt => trieOpt.orElse(Some(z.empty)).map(_ += tl) }
  }

  protected def -=(s: List[Char]): Self = s match {
    case Nil      => self.unterminate
    case hd :: tl => self.updateSubTree(hd) { trieOpt => trieOpt.map(_ -= tl).filter(_.nonEmpty) }
  }
}

// trait ListTrieTree[Self <: TrieTree[Self]](using z: Zero[Self]) extends MutableTrieTree[Self] {
//   self: Self =>
//   def +=(str: String) = mutate(self += str.toList)
//   def -=(str: String) = mutate(self -= str.toList)
//
//   @tailrec
//   protected def +=(s: List[Char]): Self = s match {
//     case Nil => self.terminate
//     case (hd :: tl) if self containsSubTree hd =>
//       val subTree = self.getSubTree(hd).get
//       subTree += tl
//     case (hd :: tl) =>
//       val subTree = self.Get
//
//   }
// }

abstract class MapTrieTree[Self <: MapTrieTree[Self]](
    map: mutable.Map[Char, Self],
    var isTerminate: Boolean
) extends MapBasedTrieTree[Self](map, isTerminate)
    with FluentMutable[Self] {
  self: Self =>
  def terminate: Self = mutate { isTerminate = true }
  def unterminate: Self = mutate { isTerminate = false }
  def updateSubTree(c: Char)(f: Option[Self] => Option[Self]): Self = mutate { map.updateWith(c)(f) }
}

class RecTrie(
    map: mutable.Map[Char, RecTrie],
    isTerminate: Boolean
) extends MapTrieTree[RecTrie](map, isTerminate)
    with RecursiveTrieTree[RecTrie]

given Zero[RecTrie] with {
  def empty = new RecTrie(mutable.Map.empty, false)
}
