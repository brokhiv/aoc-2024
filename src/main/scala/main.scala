import java.io.FileNotFoundException
import java.util.Calendar
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

trait Day extends RegexParsers {
  /**
   * Default input path. Usually of the form 'dayXX'.
   */
  val inputPath: String
  /**
   * Parsed puzzle format.
   */
  type Puzzle

  /**
   * Input parser for the puzzle.
   */
  def parsePuzzle: Parser[Puzzle]

  /**
   * Solver for part 1 of the puzzle.
   * @param puzzle the puzzle input in parsed form.
   * @return the solution.
   */
  def solve1(puzzle: Puzzle): Any

  /**
   * Solver for part 2 of the puzzle.
   *
   * @param puzzle the puzzle input in parsed form.
   * @return the solution.
   */
  def solve2(puzzle: Puzzle): Any

  /**
   * Runs the Day on a given input location.
   * It first parses the input, then runs both solvers and prints the result.
   * @param pathOption optional path to input file, relative from `src\input` and without the `.txt` extension.
   *                  If it is `None`, `this.inputPath` will be used by default.
   * @example `run(Some("example"))`: runs the parser and solvers on `src\input\example.txt`.
   */
  final def run(pathOption: Option[String] = None): (String, Long, Long) = {
    val output = new StringBuilder
    output.append(s"Running day ${ inputPath.takeRight(2) } on ${ pathOption.getOrElse("default input") }...\n")
    val inputFile = Source.fromFile(s"src\\input\\${ pathOption.getOrElse(inputPath) }.txt")
    val rawInput = inputFile.getLines().mkString("\n")
    inputFile.close()

    var (time1: Long, time2: Long) = (0L, 0L)
    val parseResult = parseAll(parsePuzzle, rawInput)
    if (parseResult.successful) {
      output.append("Completed parsing, running solver...\n")
      val puzzle: Puzzle = parseResult.get

      val start = System.nanoTime()
      val solution1 = solve1(puzzle)
      val mid = System.nanoTime()
      val solution2 = solve2(puzzle)
      val end = System.nanoTime()
      output.append(s"Part 1 solution:\n$solution1\n\n")
      output.append(s"Part 2 solution:\n$solution2\n\n")
      time1 = mid - start
      time2 = end - mid
    } else {
      output.append(s"Parsing failed, position of failure:\n${ parseResult.next.pos }\n\n")
    }

    (output.mkString, time1, time2)
  }
}

val days: List[Day] = List(
  Day01, Day02, Day03, Day04, Day05,
  Day06, Day07, Day08, Day09, Day10,
  Day11, Day12, Day13, Day14, Day15,
  Day16, Day17, Day18, Day19, Day20,
  Day21, Day22, Day23, Day24, Day25
)

@main
def main(day: Int = 0, inputPath: String = ""): Unit = {
  val pathOption = if inputPath == "" then None else Some(inputPath)
  if day == -1 then days.foreach(day => 
    try
      print(day.run(pathOption)._1)
    catch
      case _: FileNotFoundException => 
        Console.err.println(s"Could not run day ${ days.indexOf(day) + 1 }, input file is missing")
      case e: NotImplementedError   => 
        Console.err.println(s"Implementation of ${ e.getStackTrace()(1) } is missing")
  )
  else if day == 0 then print(days(Calendar.getInstance().get(Calendar.DAY_OF_MONTH) - 1).run(pathOption)._1)
  else print(days(day - 1).run(pathOption)._1)
}