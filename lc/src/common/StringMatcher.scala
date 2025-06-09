package common

import scala.collection.View

enum ViewMatcher[+T] {
  case VCons[+T](head: T, tail: View[T]) extends ViewMatcher[T]
  case VNil extends ViewMatcher[Nothing]
}

object ViewMatcher {
  def apply[T](s: View[T]): ViewMatcher[T] = if s.isEmpty then VNil else VCons(s.head, s.tail)
}
