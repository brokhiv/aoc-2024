import java.io.{ ByteArrayOutputStream, PrintStream }
import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable

private object OutputParser extends RegexParsers:
  override def skipWhitespace: Boolean = false

  private def fluff: Parser[Unit] = "[^:]*:\n".r ^^ { _ => () }

  private def solution: Parser[String] = fluff ~> "(\n?[^\n])+".r <~ "\n\n" ^^ { x => x }

  private def day: Parser[(String, String)] =
    solution ~ solution ^^ { case sol1 ~ sol2 => (sol1, sol2) }

  def allDays: Parser[List[(String, String)]] = rep(day) ^^ { days => days }

  def parseDays(input: String): List[(String, String)] =
    val result = parse(allDays, input)
    if result.successful then result.get
    else throw RuntimeException(s"Parsing failed at ${ result.next.pos }, before ${ result.next.first }")
end OutputParser

def runDayTest(dayIndex: Int): Unit = {
  def format(nanos: Long) =
    if      nanos >= 60_000_000_000L then s"${ nanos / 60_000_000_000L }:${ (nanos / 1_000_000_000L) % 60 }"
    else if nanos >=  1_000_000_000  then f"${ nanos / 1_000_000_000.0 }%.3g s"
    else if nanos >=      1_000_000  then f"${ nanos / 1_000_000.0 }%.3g ms"
    else                                  s"${ nanos / 1_000.0 } µs"

  try {
    println(s"Running tests for Day ${ dayIndex + 1 }...")
    val (rawOutput, time1, time2) = days(dayIndex).run()
    println(s"Part 1 took ${ format(time1) }, Part 2 took ${ format(time2) }")
    val output = OutputParser.parseDays(rawOutput.replace("\r", ""))
    // Compare the output with the expected solutions
    val expected = solutionsSoFar(dayIndex)
    if (output.headOption.contains(expected)) {
      println(s"Day ${ dayIndex + 1 } passed")
    } else {
      println(s"Day ${ dayIndex + 1 } failed: Expected $expected, found ${ output.headOption.getOrElse("None") }")
    }
  } catch {
    case e: Exception =>
      println(s"Error running Day ${ dayIndex + 1 }: ${ e.getMessage }")
  }
}

private val solutionsSoFar: List[(String, String)] = List(
  (2192892, 22962826), //1
  (585, 626), //2
  (174103751, 100411201), //3
  (2504, 1923), //4
  (7198, 4230), //5
  (5329, 2162), //6
  (6231007345478L, 333027885676693L), //7
  (228, 766), //8
  (6356833654075L, 6389911791746L), //9
).map((a: Any, b: Any) => (a.toString, b.toString))

@main def regressionTest(): Unit = {
  for (dayIndex <- solutionsSoFar.indices) {
    runDayTest(dayIndex)
  }
  println("Finished regression testing")
}
