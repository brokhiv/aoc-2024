
object Day08 extends Day {
  override val inputPath: String = "day08"
  override type Puzzle = (Int, Map[Char, List[(Int, Int)]])

  override def skipWhitespace: Boolean = false
  private def line: Parser[List[(Char, Int)]] =
    rep1("[A-Za-z0-9.]".r) ^^ { _.map(_.head).zipWithIndex.filterNot(_._1 == '.') }
  override def parsePuzzle: Day08.Parser[Puzzle] =
    rep1sep(line, "\n") ^^ { rs => (rs.length, rs.zipWithIndex.flatMap((cs, i) => cs.map((c, j) => (c, (i, j)))).groupMap(_._1)(_._2)) }

  private def inBounds(idx: Int, size: Int) =
    0 <= idx && idx < size

  override def solve1(puzzle: Puzzle): Any = {
    val (size, antennas) = puzzle
    antennas.view.mapValues(_.combinations(2).flatMap { case List(a1, a2) =>
      val (i1, j1) = a1
      val (i2, j2) = a2
      val n1 = (2*i1 - i2, 2*j1 - j2)
      val n2 = (2*i2 - i1, 2*j2 - j1)
      List(n1, n2).filter{ (i, j) => inBounds(i, size) && inBounds(j, size) }
    }).values.flatten.toSet.size
  }

  override def solve2(puzzle: Puzzle): Any = {
    val (size, antennas) = puzzle
    antennas.view.mapValues(_.combinations(2).flatMap { case List(a1, a2) =>
      val (i1, j1) = a1
      val (i2, j2) = a2
      val ns = for k <- -size + 1 until size yield ((k+1) * i1 - k * i2, (k+1) * j1 - k * j2)
      ns.filter { (i, j) => inBounds(i, size) && inBounds(j, size) }
    }).values.flatten.toSet.size
  }
}
