
object Day07 extends Day {
  override val inputPath: String = "day07"
  override type Puzzle = List[(Long, List[Long])]

  override def skipWhitespace: Boolean = false
  private def number: Day07.Parser[Long] = """\d+""".r ^^ { _.toLong }
  private def line: Parser[(Long, List[Long])] =
    (number <~ ": ") ~ rep1sep(number, " ") ^^ { case t ~ ns => (t, ns) }
  override def parsePuzzle: Day07.Parser[Puzzle] = rep1sep(line, "\n")

  override def solve1(puzzle: Puzzle): Any = {
    puzzle.sumBy { (t, ns) =>
      if ns.tail.foldLeft(List(ns.head)) {
        (cs, n) => cs.flatMap(c => List(c + n, c * n))
      }.contains(t)
      then t else 0L
    }
  }

  override def solve2(puzzle: Puzzle): Any = {
    puzzle.sumBy { (t, ns) =>
      if ns.tail.foldLeft(List(ns.head)) {
        (cs, n) => cs.flatMap(c => List(c + n, c * n, (c.toString + n.toString).toLong))
      }.contains(t)
      then t else 0L
    }
  }
}
