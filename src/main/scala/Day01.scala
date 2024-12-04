object Day01 extends Day {
  override val inputFile: String = "day01"
  type Puzzle = (List[Int], List[Int])

  def number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ {
    _.toInt
  }

  def line: Parser[(Int, Int)] = number ~ number ^^ { case a ~ b => (a, b) }

  override def parsePuzzle: Parser[Puzzle] = rep1(line) ^^ {
    _.unzip
  }

  override def solve1(puzzle: Puzzle): Int = {
    val (left, right) = puzzle
    left.sorted
      .lazyZip(right.sorted)
      .map((a, b) => if (a > b) a - b else b - a)
      .sum
  }

  override def solve2(puzzle: Puzzle): Int = {
    val (left, right) = puzzle
    left.map(it =>
      it * right.count(_ == it)
    ).sum
  }
}
