import java.nio.file.{Files, Paths, StandardOpenOption}

val template = """
object DayXX extends Day {
  override val inputFile: String = "dayXX"
  override type Puzzle = Nothing

  override def parsePuzzle: DayXX.Parser[Puzzle] = ???

  override def solve1(puzzle: Puzzle): Any = ???

  override def solve2(puzzle: Puzzle): Any = ???
}
""".stripPrefix("\n")

@main def fill(): Unit = {
  for d <- 1 to 25 do {
    val content = template.replace("XX", f"$d%02d")
    val fileName =  f"src\\main\\scala\\Day$d%02d.scala"
    if (!Files.exists(Paths.get(fileName))) then
      Files.write(Paths.get(fileName), content.getBytes, StandardOpenOption.CREATE)
  }
}