import scala.annotation.tailrec

object Day09 extends Day {
  override val inputPath: String = "day09"
  override type Puzzle = (List[Int], List[Int])

  private def digit: Parser[Int] = "\\d".r ^^ { _.toInt }
  override def parsePuzzle: Day09.Parser[Puzzle] =
    digit ~ rep(digit) ^^ { case f1 ~ sfs =>
      val (ss, fs) = sfs.grouped(2).map { case List(e, f) => (e, f) }.toList.unzip
      (f1::fs, ss)
  }

  override def solve1(puzzle: Puzzle): Any = {
    @tailrec
    def go(fileBlocks: List[(Int, Int)], spaces: List[Int], acc: List[(Int, Int)]): List[(Int, Int)] = {
      //      require(fileBlocks.sumBy(_._1) + acc.sumBy(_._1) == totalFileBlocks)
      //      require(fileBlocks.length == spaces.length + 1)
      //      require(flattenAndCount(acc ++ fileBlocks) == fileFrequencies)
      //      require(fileBlocks.forall(_._1 > 0))
      //      require(acc.forall(_._1 > 0))
      //      require(spaces.forall(_ >= 0))
      if spaces.isEmpty || fileBlocks.isEmpty
      then acc.reverse ++ fileBlocks
      else
        val firstSpace = spaces.head
        val (count, index) = fileBlocks.last
        if firstSpace == 0 then
          go(fileBlocks.tail, spaces.tail, fileBlocks.head :: acc)
        else if count < firstSpace then
          go((count, index) :: fileBlocks.tail.dropRight(1), ((firstSpace - count) :: spaces.tail).dropRight(1), fileBlocks.head :: acc)
        else
          val remainingBlocks = if count == firstSpace
          then List.empty
          else List((count - firstSpace, index))
          go(fileBlocks.tail.dropRight(1) ++ remainingBlocks, spaces.tail.dropRight(if count == firstSpace then 1 else 0), (firstSpace, index) :: fileBlocks.head :: acc)
    }

    val (files, spaces) = puzzle
    val compacted = go(files.zipWithIndex, spaces, List.empty)
    compacted
      .flatMap { (count, index) => List.fill(count)(index) }
      .zipWithIndex
      .sumBy { (id, pos) => (id * pos).toLong }
  }

  extension [A](xs: List[A])
    def moveLeft(from: Int, to: Int): List[A] =
        require(from >= to)
        xs.take(to) ++ (xs(from) :: xs.slice(to, from) ++ xs.drop(from + 1))

  override def solve2(puzzle: Puzzle): Any = {
    @tailrec
    def go(fileBlocks: List[(Int, Int)], spaces: List[Int], fileToMove: Int): List[(Int, Int)] = {
//      if fileToMove % 100 == 0 then println(fileToMove)
      if fileToMove == 0
      then fileBlocks.lazyZip(spaces).flatMap { (fb, s) => List(fb, (s, 0)) }
      else
        val moveIndex = fileBlocks.indexWhere(_._2 == fileToMove)
        val availableSpace = spaces.take(moveIndex).indexWhere(_ >= fileBlocks.find(_._2 == fileToMove).get._1)
        val (fileBlocks_, spaces_) = availableSpace match
          case -1 => (fileBlocks, spaces)
          case iSpace =>
            val before = spaces.take(iSpace)
            val fileCount = fileBlocks(moveIndex)._1
            val between = spaces.slice(iSpace + 1, moveIndex - 1)
            val filledSpaces = if iSpace + 1 != moveIndex then List(0, spaces(iSpace) - fileCount) else List(0)
            val emptiedSpace = if iSpace + 1 != moveIndex then spaces(moveIndex - 1) + fileCount + spaces(moveIndex) else spaces(iSpace) + spaces(moveIndex)
            val after = spaces.drop(moveIndex + 1)
            val newSpaces = before ++ filledSpaces ++ between ++ (emptiedSpace :: after)
            (fileBlocks.moveLeft(moveIndex, iSpace + 1), newSpaces)
        go(fileBlocks_, spaces_, fileToMove - 1)
    }

    val (files, spaces) = puzzle
    val compacted = go(files.zipWithIndex, spaces ++ List(0), files.length - 1)
    compacted
      .flatMap { (count, index) => List.fill(count)(index) }
      .zipWithIndex
      .sumBy { (id, pos) => (id * pos).toLong }
  }
}
