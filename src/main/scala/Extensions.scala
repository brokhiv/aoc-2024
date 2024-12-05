
extension [A](xs: Iterable[A])
  def sumBy[B](f: A => B)(using Numeric[B]): B =
    xs.map(f).sum

extension [A](xs: Iterator[A])
  def sumBy[B](f: A => B)(using Numeric[B]): B =
    xs.map(f).sum

extension [A](xs: IndexedSeq[A])
  def lastIndex: Int = xs.length - 1

extension[A] (xs: List[A])
  def lastIndex: Int = xs.length - 1