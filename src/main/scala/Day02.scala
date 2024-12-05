import scala.annotation.nowarn

object Day02 extends Day {
  override val inputPath: String = "day02"
  type Puzzle = List[List[Int]]

  override def skipWhitespace: Boolean = false

  def number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }

  def line: Parser[List[Int]] = rep1sep(number, " +".r)

  override def parsePuzzle: Parser[Puzzle] = rep1sep(line, "\n".r)

  @nowarn("msg=match may not be exhaustive")
  private def checkSafe(report: List[Int]): Boolean = {
    val checkSorted = report.sliding(2)
      .map { case List(a, b) => a < b }
      .distinct
      .length
      == 1
    val checkDiffs = report.sliding(2)
      .map { case List(a, b) => if a > b then a - b else b - a }
      .forall(d => 1 <= d && d <= 3)
    checkSorted && checkDiffs
  }

  override def solve1(puzzle: Day02.Puzzle): Int = {
    puzzle.count(checkSafe)
  }

  override def solve2(puzzle: Day02.Puzzle): Int = {
    def dropOne(report: List[Int]): Iterable[List[Int]] =
      for i <- report.indices yield report.take(i) ++ report.drop(i + 1)

    puzzle.count(report => dropOne(report).exists(checkSafe))
  }
}
