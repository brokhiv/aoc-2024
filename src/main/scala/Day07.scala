
object Day07 extends Day {
  override val inputPath: String = "day07"
  override type Puzzle = List[(BigInt, List[BigInt])]

  override def skipWhitespace: Boolean = false
  private def number: Parser[BigInt] = """\d+""".r ^^ BigInt.apply
  private def line: Parser[(BigInt, List[BigInt])] =
    (number <~ ": ") ~ rep1sep(number, " ") ^^ { case t ~ ns => (t, ns) }
  override def parsePuzzle: Day07.Parser[Puzzle] = rep1sep(line, "\n")

  override def solve1(puzzle: Puzzle): Any = {
    puzzle.sumBy { (t, ns) =>
      if ns.tail.foldLeft(Set(ns.head)) {
        (cs, n) => cs.flatMap(c => List(c + n, c * n))
      }.contains(t)
      then t else BigInt(0)
    }
  }

  override def solve2(puzzle: Puzzle): Any = {
    puzzle.sumBy { (t, ns) =>
      if ns.tail.foldLeft(Set(ns.head)) {
        (cs, n) => cs.flatMap(c => List(c + n, c * n, BigInt(c.toString + n.toString)))
      }.contains(t)
      then t else BigInt(0)
    }
  }
}
