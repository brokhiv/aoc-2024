
extension [A](xs: IterableOnce[A])
  def sumBy[B](f: A => B)(using Numeric[B]): B =
    xs.iterator.map(f).sum

extension [A](xs: Seq[A])
  def lastIndex: Int = xs.length - 1
