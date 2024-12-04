import java.util.Calendar
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

trait Day extends RegexParsers {
  val inputFile: String
  type Puzzle
  def parsePuzzle: Parser[Puzzle]

  def solve1(puzzle: Puzzle): Any
  def solve2(puzzle: Puzzle): Any
}

def run[D <: Day](day: D, input: String = ""): Unit = {
  import day.parse

  val inputFile = Source.fromFile(s"src\\input\\${if input == "" then day.inputFile else input}.txt")
  val rawInput = inputFile.getLines().mkString("\n")
  inputFile.close()
  val parseResult = parse(day.parsePuzzle, rawInput)
  if (parseResult.successful) {
    val puzzle: day.Puzzle = parseResult.get
    println(day.solve1(puzzle))
    println(day.solve2(puzzle))
  } else {
    println(s"Parsing failed, position of failure:\n${parseResult.next.pos}")
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
def main(day: Int = 0, inputFile: String = ""): Unit = {
  if day == -1 then days.foreach(day => run(day, inputFile))
  else if day == 0 then run(days(Calendar.getInstance().get(Calendar.DAY_OF_MONTH) - 1), inputFile)
  else run(days(day - 1), inputFile)
}