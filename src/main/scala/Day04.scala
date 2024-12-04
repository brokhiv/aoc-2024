
object Day04 extends Day {
  override val inputFile: String = "day04"
  override type Puzzle = List[List[Char]]

  override def skipWhitespace: Boolean = false
  private def char: Parser[Char] = "[XMAS]".r ^^ { _.head }
  override def parsePuzzle: Parser[Puzzle] = rep1sep(rep(char), "\n")

  override def solve1(puzzle: Puzzle): Any = {
    val XMAS = "XMAS".toList
    def checkRows(grid: Iterable[List[Char]]): Int = grid
      .map(row => row.sliding(XMAS.length).count(word => word == XMAS || word.reverse == XMAS))
      .sum

    def diagonals(grid: Puzzle, isMainDiagonal: Boolean): Iterable[List[Char]] = {
      val range = if (isMainDiagonal) 0 to 2 * (grid.length - 1) else -(grid.length - 1) to (grid.length - 1)
      for
        k <- range
      yield for
        (row, i) <- grid.zipWithIndex
        (col, j) <- row.zipWithIndex
        if (if (isMainDiagonal) k == i + j else k == i - j)
      yield col
    }

    val rows = checkRows(puzzle)
    val cols = checkRows(puzzle.transpose)
    val mainDiags = checkRows(diagonals(puzzle, true))
    val antiDiags = checkRows(diagonals(puzzle, false))

    rows + cols + mainDiags + antiDiags
  }

  override def solve2(puzzle: Puzzle): Any = {
    def checkBlock(block: List[Char]): Boolean = block match
      case List('M', _, 'M', _, 'A', _, 'S', _, 'S') => true
      case List('M', _, 'S', _, 'A', _, 'M', _, 'S') => true
      case List('S', _, 'M', _, 'A', _, 'S', _, 'M') => true
      case List('S', _, 'S', _, 'A', _, 'M', _, 'M') => true
      case _ => false

    val candidates = puzzle
      .sliding(3)
      .map(_.map(_.sliding(3).toList))
      .map(_.transpose)
      .map(_.map(_.flatten))
    
    candidates.map(block => block.count(checkBlock)).sum
  }
}
