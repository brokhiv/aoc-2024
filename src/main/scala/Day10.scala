
object Day10 extends Day {
  override val inputPath: String = "day10"
  override type Puzzle = Vector[Vector[Int]]

  override def skipWhitespace: Boolean = false
  private def height: Parser[Int] = "\\d".r ^^ (_.toInt)
  private def line: Parser[Vector[Int]] = rep1(height) ^^ (_.toVector)
  override def parsePuzzle: Day10.Parser[Puzzle] = rep1sep(line, "\n") ^^ (_.toVector)

  private def neighbors(size: Int)(pos: (Int, Int)): List[(Int, Int)] =
    val (i, j) = pos
    List((i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1))
      .filter{ (i, j) => 0 <= i && i < size && 0 <= j && j < size }

  private def search(grid: Puzzle)(level: Int = 0)(root: (Int, Int)): List[(Int, Int)] = {
    if level == 9 then List(root)
    else neighbors(grid.length)(root)
      .filter { (i, j) => grid(i)(j) == level + 1 }
      .flatMap(search(grid)(level + 1))
  }

  override def solve1(puzzle: Puzzle): Any = {
    val trailHeads = puzzle.zipWithIndex.flatMap { (row, i) =>
      row.zipWithIndex.filter(_._1 == 0).map { (_, j) => (i, j) }
    }
    trailHeads
      .map(search(puzzle)(0))
      .sumBy(_.distinct.length)
  }

  override def solve2(puzzle: Puzzle): Any = {
    val trailHeads = puzzle.zipWithIndex.flatMap { (row, i) =>
      row.zipWithIndex.filter(_._1 == 0).map { (_, j) => (i, j) }
    }
    trailHeads
      .map(search(puzzle)(0))
      .sumBy(_.length)
  }
}
