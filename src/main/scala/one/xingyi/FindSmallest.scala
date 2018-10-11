package one.xingyi

object FindSmallest {
  def apply[T](fn: T => Int)(s: Seq[T]): T = s.sortBy(fn).head


}
