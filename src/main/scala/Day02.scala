object Day02 extends Day {
  override val inputFile: String = "day02"
  type Puzzle = List[List[Int]]
  
  override def skipWhitespace: Boolean = false
  def number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def line: Parser[List[Int]] = rep1sep(number, " +".r)
  override def parsePuzzle: Parser[Puzzle] = rep1sep(line, "\n".r)
  
  private def checkSafe(report: List[Int]): Boolean = {
    val windowed = report.sliding(2).toList
    val checkSorted = windowed.map({ case a :: b :: _ => a < b }).distinct.length == 1
    val diffs = windowed.map({ case a :: b :: _ => if a > b then a - b else b - a })
    val checkDiffs = diffs.min >= 1 && diffs.max <= 3
    checkSorted && checkDiffs
  }

  override def solve1(puzzle: Day02.Puzzle): Int = {
    puzzle.count(checkSafe)
  }

  override def solve2(puzzle: Day02.Puzzle): Int = {
    def dropOne(report: List[Int]): IndexedSeq[List[Int]] =
      for i <- report.indices yield report.take(i) ++ report.drop(i + 1)

    val (safe, unsafe) = puzzle.partition(checkSafe)
    val dampened = unsafe.count(report => dropOne(report).exists(checkSafe))
    safe.length + dampened
  }

//  @main def day02(): Unit = {
//    val rawInput = Source.fromFile("src\\input\\day02.txt").getLines().mkString("\n")
//    val parseResult = parseAll(Day02Parser.day02, rawInput)
//    if (parseResult.successful) {
//      val puzzle: Puzzle = parseResult.get
//      println(solve1(puzzle))
//      println(solve2(puzzle))
//    } else {
//      println(s"Parsing failed, position of failure:\n${parseResult.next.pos}")
//    }
//  }
}
