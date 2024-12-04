object Day01 extends Day {
  override val inputPath: String = "day01"
  type Puzzle = (List[Int], List[Int])

  def number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }

  def line: Parser[(Int, Int)] = number ~ number ^^ { case a ~ b => (a, b) }

  override def parsePuzzle: Parser[Puzzle] = rep1(line) ^^ { _.unzip }

  override def solve1(puzzle: Puzzle): Int = {
    val (left, right) = puzzle
    left.sorted
      .lazyZip(right.sorted)
      .sumBy((a, b) => if (a > b) a - b else b - a)
  }

  override def solve2(puzzle: Puzzle): Int = {
    val (left, right) = puzzle
    left.sumBy(a => a * right.count(_ == a))
  }
}
