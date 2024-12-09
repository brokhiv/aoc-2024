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

  private def moveBlocks(fileBlocks: List[(Int, Int)], spaces: List[Int]): List[(Int, Int)] = {
    val totalFileBlocks = fileBlocks.sumBy(_._1)
    val flattenAndCount = (xs: List[(Int, Int)]) => xs.flatMap { case (count, value) => List.fill(count)(value) }.groupBy(identity).view.mapValues(_.size).toMap
    val fileFrequencies = flattenAndCount(fileBlocks)

    @tailrec
    def go(fileBlocks: List[(Int, Int)], spaces: List[Int], acc: List[(Int, Int)]): List[(Int, Int)] = {
//      println(acc.take(10).reverse)
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

    go(fileBlocks, spaces, List.empty)
  }

  override def solve1(puzzle: Puzzle): Any = {
    val (files, spaces) = puzzle
    val compacted = moveBlocks(files.zipWithIndex, spaces)
    compacted
      .flatMap { (count, index) => List.fill(count)(index) }
      .zipWithIndex
      .sumBy { (id, pos) => id * pos }
  }

  override def solve2(puzzle: Puzzle): Any = ???
}
