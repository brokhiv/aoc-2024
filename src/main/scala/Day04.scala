
object Day04 extends Day {
  override val inputPath: String = "day04"
  override type Puzzle = List[List[Char]]

  override def skipWhitespace: Boolean = false

  private def char: Parser[Char] = "[XMAS]".r ^^ { _.head }

  override def parsePuzzle: Parser[Puzzle] = rep1sep(rep(char), "\n")

  override def solve1(puzzle: Puzzle): Any = {
    val XMAS = "XMAS".toList

    def checkRows(grid: Iterable[List[Char]]): Int = grid
      .sumBy(_.sliding(XMAS.length).count(word => word == XMAS || word.reverse == XMAS))

    def diagonals(grid: Puzzle, isMainDiagonal: Boolean): Iterable[List[Char]] = {
      for
        k <- if isMainDiagonal then 0 to 2 * grid.lastIndex else -grid.lastIndex to grid.lastIndex
      yield for
        (row, i) <- grid.zipWithIndex
        (col, j) <- row.zipWithIndex
        if isMainDiagonal && k == i + j || !isMainDiagonal && k == i - j
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
      case List('M', _, 'M', _, 'A', _, 'S', _, 'S') |
           List('M', _, 'S', _, 'A', _, 'M', _, 'S') |
           List('S', _, 'M', _, 'A', _, 'S', _, 'M') |
           List('S', _, 'S', _, 'A', _, 'M', _, 'M') => true
      case _                                         => false

    puzzle
      .sliding(3)
      .sumBy(_.map(_.sliding(3).toList)
        .transpose
        .map(_.flatten)
        .count(checkBlock)
      )
  }
}
