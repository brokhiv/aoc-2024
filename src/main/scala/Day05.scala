
object Day05 extends Day {
  override val inputPath: String = "day05"
  override type Puzzle = (Set[(Int, Int)], List[List[Int]])

  private def number: Parser[Int] = "\\d+".r ^^ { _.toInt }
  private def rule: Parser[(Int, Int)] = number ~ "|" ~ number ^^ { case  a ~ _ ~ b => (a, b) }
  private def update: Parser[List[Int]] = rep1sep(number, ",")
  override def parsePuzzle: Day05.Parser[Puzzle] =
    rep1(rule) ~ rep1(update) ^^ { case rules ~ updates => (rules.toSet, updates) }

  private def checkOrdering(rules: Set[(Int, Int)])(xs: List[Int]) =
    xs.sliding(2).forall { case List(a, b) => !rules.contains((b, a)) }

  override def solve1(puzzle: Puzzle): Any = {
    val (rules, updates) = puzzle
    updates
      .filter(checkOrdering(rules))
      .sumBy(update => update(update.length / 2))
  }

  override def solve2(puzzle: Puzzle): Any = {
    val (rules, updates) = puzzle
    updates
      .filterNot(checkOrdering(rules))
      .map(_.sortWith((a, b) => rules.contains((a, b))))
      .sumBy(update => update(update.length / 2))
  }
}
