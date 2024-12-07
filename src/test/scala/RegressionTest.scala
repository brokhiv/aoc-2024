import java.io.{ ByteArrayOutputStream, PrintStream }
import scala.util.parsing.combinator.RegexParsers

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
    else throw RuntimeException(s"Parsing failed at ${result.next.pos}, before ${result.next.first}")
end OutputParser


private val solutionsSoFar: List[(String, String)] = List(
  (2192892, 22962826),                //1
  (585, 626),                         //2
  (174103751, 100411201),             //3
  (2504, 1923),                       //4
  (7198, 4230),                       //5
  (5329, 2162),                       //6
  (6231007345478L, 333027885676693L), //7
).map((a: Any, b: Any) => (a.toString, b.toString))

@main def regressionTest(): Unit = {
  val outputStream = new ByteArrayOutputStream()
  val printStream = new PrintStream(outputStream)
  val originalOut = System.out

  try {
    System.setOut(printStream)

    main(-1)

    val output = OutputParser.parseDays(outputStream.toString())
    output.lazyZip(solutionsSoFar)
      .foreach { (out, expected) => assert(out == expected, s"Expected $expected, found $out") }
  } finally {
    System.setOut(originalOut)
  }
}