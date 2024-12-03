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

//  @main def day01(): Unit = {
//    val rawInput = Source.fromFile("src\\input\\day01.txt").getLines().mkString("\n")
//    val parseResult = parseAll(Day01Parser.day01, rawInput)
//    if (parseResult.successful) {
//      val puzzle: Puzzle = parseResult.get
//      println(solve1(puzzle))
//      println(solve2(puzzle))
//    } else {
//      println(s"Parsing failed, position of failure:\n${parseResult.next.pos}")
