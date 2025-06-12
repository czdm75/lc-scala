package common

import scala.collection.immutable.Queue

trait BiTree[T, Self <: BiTree[T, Self]] {
  self: Self =>
  def leftChild: Option[Self]
  def rightChild: Option[Self]
  def element: T

  def foldTree[R](init: R)(f: (T, R, R) => R): R = {
    val left = leftChild.map(_.foldTree(init)(f)).getOrElse(init)
    val right = rightChild.map(_.foldTree(init)(f)).getOrElse(init)
    f(element, left, right)
  }

  def infixTraverse: Seq[T]
  def prefixTraverse: Seq[T]
  def postfixTraverse: Seq[T]
  def breadthFirstTraverse: Seq[T]
}

trait RecursiveConcatTraverse[T, Self <: RecursiveConcatTraverse[T, Self]] extends BiTree[T, Self] {
  self: Self =>

  def infixTraverse: Seq[T] = foldTree(List.empty[T]) { case (elem, left, right) => left ++ (elem :: right) }

  def prefixTraverse: Seq[T] = foldTree(List.empty[T]) { case (elem, left, right) => elem :: left ++ right }

  def postfixTraverse: Seq[T] = foldTree(List.empty[T]) { case (elem, left, right) => left ++ right ++ List(elem) }

  def breadthFirstTraverse: Seq[T] = foldTree(List.empty[List[T]]) { case (elem, left, right) =>
    List(elem) :: left.zipAll(right, List.empty[T], List.empty[T]).map(_ ++ _)
  }.flatten
}

trait RecursiveTraverse[T, Self <: RecursiveTraverse[T, Self]] extends BiTree[T, Self] {
  self: Self =>

  // CPS concating into prepending to List

  extension [A](x: A) def applyIfPresent[F](opt: Option[F])(f: (F, A) => A): A = opt.map(f(_, x)).getOrElse(x)

  def infixTraverse: Seq[T] = infixTraverse(List.empty)

  private def infixTraverse(result: List[T]): List[T] = {
    result
      .applyIfPresent(leftChild)(_ infixTraverse _)
      .prepended(element)
      .applyIfPresent(rightChild)(_ infixTraverse _)
  }

  def prefixTraverse: Seq[T] = prefixTraverse(List.empty)

  private def prefixTraverse(result: List[T]): List[T] = {
    result
      .prepended(element)
      .applyIfPresent(leftChild)(_ infixTraverse _)
      .applyIfPresent(rightChild)(_ infixTraverse _)
  }

  def postfixTraverse: Seq[T] = postfixTraverse(List.empty)

  private def postfixTraverse(result: List[T]): List[T] = {
    result
      .applyIfPresent(leftChild)(_ infixTraverse _)
      .applyIfPresent(rightChild)(_ infixTraverse _)
      .prepended(element)
  }

  def breadthFirstTraverse: Seq[T] = ???
}

trait ListTraverse[T, Self <: ListTraverse[T, Self]] extends BiTree[T, Self] {
  self: Self =>

  import ListTraverse.TState._

  def breadthFirstTraverse: Seq[T] = ListTraverse.breadthFirstTraverse(Queue(self), Nil)

  def infixTraverse: Seq[T] = ListTraverse.infixTraverse(List((TLeft, self)), Nil)
  def prefixTraverse: Seq[T] = ListTraverse.prefixTraverse(List((TSelf, self)), Nil)
  def postfixTraverse: Seq[T] = ListTraverse.postfixTraverse(List((TLeft, self)), Nil)
}

object ListTraverse {
  import scala.annotation.tailrec

  extension [T](x: Queue[T]) def :+?(opt: Option[T]) = opt.map(x :+ _).getOrElse(x)

  @tailrec
  def breadthFirstTraverse[T, Self <: ListTraverse[T, Self]](queue: Queue[Self], result: List[T]): Seq[T] = {
    queue match {
      case hd +: tl => breadthFirstTraverse(tl :+? hd.leftChild :+? hd.rightChild, hd.element :: result)
      case _        => result.reverse
    }
  }

  enum TState {
    case TLeft
    case TRight
    case TSelf
  }

  import TState._

  // very confusing...
  extension [T](opt: Option[T]) def ?::(x: List[T]) = opt.map(_ :: x).getOrElse(x)

  @tailrec
  def infixTraverse[T, Self <: ListTraverse[T, Self]](path: List[(TState, Self)], result: List[T]): Seq[T] =
    path match {
      case Nil                => result.reverse
      case (TLeft, hd) :: tl  => infixTraverse(hd.leftChild.map((TLeft, _)) ?:: ((TSelf, hd) :: tl), result)
      case (TSelf, hd) :: tl  => infixTraverse((TRight, hd) :: tl, hd.element :: result)
      case (TRight, hd) :: tl => infixTraverse(hd.rightChild.map((TLeft, _)) ?:: tl, result)
    }

  @tailrec
  def prefixTraverse[T, Self <: ListTraverse[T, Self]](path: List[(TState, Self)], result: List[T]): Seq[T] =
    path match {
      case Nil                => result.reverse
      case (TSelf, hd) :: tl  => prefixTraverse((TLeft, hd) :: tl, hd.element :: result)
      case (TLeft, hd) :: tl  => prefixTraverse(hd.leftChild.map((TLeft, _)) ?:: tl, result)
      case (TRight, hd) :: tl => prefixTraverse(hd.rightChild.map((TLeft, _)) ?:: tl, result)
    }

  @tailrec
  def postfixTraverse[T, Self <: ListTraverse[T, Self]](path: List[(TState, Self)], result: List[T]): Seq[T] =
    path match {
      case Nil                => result.reverse
      case (TLeft, hd) :: tl  => postfixTraverse(hd.leftChild.map((TLeft, _)) ?:: tl, result)
      case (TRight, hd) :: tl => postfixTraverse(hd.rightChild.map((TLeft, _)) ?:: tl, result)
      case (TSelf, hd) :: tl  => postfixTraverse(tl, hd.element :: result)
    }
}
