object Day03 extends Day {

  import Instruction._
  
  override val inputFile: String = "day03"
  
  enum Instruction {
    case Mul(val a: Int, val b: Int) extends Instruction
    case DoDont(isDo: Boolean) extends Instruction
    case Garbage extends Instruction
  }

  type Puzzle = List[Instruction]

  override def skipWhitespace: Boolean = false
  def number: Parser[Int] = """\d{1,3}""".r ^^ { _.toInt }
  def mul: Parser[Instruction] = "mul(" ~> number ~ "," ~ number <~ ")" ^^ { case a ~ _ ~ b => Mul(a, b) }
  def doDont: Parser[Instruction] = "do(n't)?".r ^^ (instr => DoDont(instr == "do"))
  def garbage: Parser[Instruction] = ".|\n".r ^^ { case _ => Garbage }
  override def parsePuzzle: Parser[Puzzle] = rep(mul | doDont | garbage)
  
  override def solve1(puzzle: Day03.Puzzle) = {
    puzzle.map {
      case Instruction.Mul(a, b) => a * b
      case _ => 0
    }.sum
  }

  override def solve2(puzzle: Day03.Puzzle) = {
    puzzle.foldLeft((true, 0)) {
      case ((_, sum), Instruction.DoDont(isDo)) => (isDo, sum)
      case ((true, sum), Instruction.Mul(a, b)) => (true, sum + a * b)
      case (acc, _) => acc
    }._2
  }
}
