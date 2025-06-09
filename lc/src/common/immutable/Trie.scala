package common.immutable

import scala.annotation.tailrec
import common.{Zero, TrieTree, ImmutableTrieTree, MapBasedTrieTree}

// implementation of modification, with a recursive approach
trait RecursiveTrieTree[Self <: RecursiveTrieTree[Self]](using z: Zero[Self]) extends ImmutableTrieTree[Self] {
  self: Self =>
  def +(str: String): Self = self + str.toList
  def -(str: String): Self = self - str.toList

  protected def +(s: List[Char]): Self = s match {
    case Nil        => self.terminated
    case (hd :: tl) => self.updatedSubTree(hd) { trieOpt => trieOpt.orElse(Some(z.empty)).map(_ + tl) }
  }

  protected def -(s: List[Char]): Self = s match {
    case Nil        => self.unterminated
    case (hd :: tl) => self.updatedSubTree(hd) { trieOpt => trieOpt.map(_ - tl).filter(_.nonEmpty) }
  }
}

// implementation of modification, with a list-based approach
object ListTrieTree {
  @tailrec
  def buildUp[Self <: ListTrieTree[Self]](node: Self, parents: List[(Char, Self)]): Self = parents match {
    case Nil           => node
    case (c, hd) :: tl => buildUp(hd.updatedSubTree(c) { _ => Some(hd).filter(_.nonEmpty) }, tl)
  }

  @tailrec
  def addElem[Self <: ListTrieTree[Self]](
      root: Self,
      s: List[Char],
      node: Self,
      parents: List[(Char, Self)]
  )(using z: Zero[Self]): Self = (s, node.isTerminate) match {
    case (Nil, true)   => root // no change shortcut
    case (Nil, false)  => buildUp(node.terminated, parents) // end recursive, build up
    case (hd :: tl, _) => addElem(root, tl, node.getSubTree(hd).getOrElse(z.empty), (hd, node) :: parents)
  }

  @tailrec
  def removeElem[Self <: ListTrieTree[Self]](root: Self, s: List[Char], node: Self, parents: List[(Char, Self)]): Self =
    (s, node.isTerminate) match {
      case (Nil, false) => root // no change shortcut
      case (Nil, _)     => buildUp(node.unterminated, parents)
      case (hd :: tl, _) if node containsSubTree hd =>
        removeElem(root, tl, node.getSubTree(hd).get, (hd, node) :: parents)
      case (hd :: tl, _) => root // no change shortcut
    }
}

trait ListTrieTree[Self <: ListTrieTree[Self]](using z: Zero[Self]) extends ImmutableTrieTree[Self] {
  self: Self =>
  def +(str: String): Self = ListTrieTree.addElem(self, str.toList, self, Nil)
  def -(str: String): Self = ListTrieTree.removeElem(self, str.toList, self, Nil)
}

trait Factory[T <: MapBasedTrieTree[_]] extends Zero[T] {
  def create(map: Map[Char, T], isTerminate: Boolean): T
}

abstract class MapTrieTree[Self <: MapTrieTree[Self]](
    map: Map[Char, Self],
    isTerminate: Boolean
)(using factory: Factory[Self])
    extends MapBasedTrieTree[Self](map, isTerminate) {
  self: Self =>
  def terminated = factory.create(map, true)
  def unterminated = factory.create(map, false)
  def updatedSubTree(c: Char)(f: Option[Self] => Option[Self]) = factory.create(map.updatedWith(c)(f), isTerminate)
}

case class RecTrie(map: Map[Char, RecTrie], isTerminate: Boolean)
    extends MapTrieTree[RecTrie](map, isTerminate)
    with RecursiveTrieTree[RecTrie]

given Factory[RecTrie] with {
  def empty = RecTrie(Map.empty, false)
  def create(map: Map[Char, RecTrie], isTerminate: Boolean) = RecTrie(map, isTerminate)
}

case class Trie(map: Map[Char, Trie], isTerminate: Boolean)
    extends MapTrieTree[Trie](map, isTerminate)
    with ListTrieTree[Trie]

given Factory[Trie] with {
  def empty = Trie(Map.empty, false)
  def create(map: Map[Char, Trie], isTerminate: Boolean) = Trie(map, isTerminate)
}
