package common

trait Zero[T] {
  def empty: T
}

trait FluentMutable[Self <: FluentMutable[Self]] {
  self: Self =>
  protected def mutate(f: => Unit): Self = { f; self }
}
