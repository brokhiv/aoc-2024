import scala.collection.mutable
import scala.math.abs

object Day06 extends Day {
  override val inputPath: String = "day06"
  private[Day06] enum Direction:
    case UP extends Direction
    case RIGHT extends Direction
    case DOWN extends Direction
    case LEFT extends Direction

    def next: Direction = this match
      case UP => RIGHT
      case RIGHT => DOWN
      case DOWN => LEFT
      case LEFT => UP

  private[Day06] object Direction extends IterableOnce[Direction]:
    override def iterator: Iterator[Direction] = List(UP, RIGHT, DOWN, LEFT).iterator

    def apply(arrow: Char): Direction = (arrow: @unchecked) match {
      case '^' => UP
      case '>' => RIGHT
      case 'v' => DOWN
      case '<' => LEFT
    }

  override type Puzzle = (Set[(Int, Int)], (Int, Int), Direction)

  override def skipWhitespace: Boolean = false
  private def position: Parser[Char] = "[.#^>v<]".r ^^ { x => x.head }
  private def line: Parser[List[Char]] = rep1(position)
  private def extractInfo(lines: List[List[Char]]): Puzzle = {
    val withPositions = lines.zipWithIndex
      .flatMap((row, i) => row.zipWithIndex.map((c, j) => (c, i, j)))
    val obstacles = withPositions.collect { case ('#', i, j) => (i, j) }.toSet
    val (guardPos, guardDir) = withPositions.find(x => "^>v<".contains(x._1))
      .map((d, i, j) => ((i, j), Direction(d))).get
    (obstacles, guardPos, guardDir)
  }
  override def parsePuzzle: Parser[Puzzle] = rep1sep(line, "\n") ^^ extractInfo

  private def sees(guardPos: (Int, Int), guardDir: Direction)(otherPos: (Int, Int)): Boolean =
    guardDir match
      case Direction.UP    => guardPos._1 > otherPos._1 && guardPos._2 == otherPos._2
      case Direction.RIGHT => guardPos._2 < otherPos._2 && guardPos._1 == otherPos._1
      case Direction.DOWN  => guardPos._1 < otherPos._1 && guardPos._2 == otherPos._2
      case Direction.LEFT  => guardPos._2 > otherPos._2 && guardPos._1 == otherPos._1

  private def manhattan(from: (Int, Int))(to: (Int, Int)): Int = abs(to._1 - from._1) + abs(to._2 - from._2)

  override def solve1(puzzle: Puzzle): Any = {
    var (obstacles, guardPos, guardDir) = puzzle
    val bounds: Map[Direction, Int] = Map(
      (Direction.UP, -1),
      (Direction.RIGHT, obstacles.maxBy(_._2)._2 + 1),
      (Direction.DOWN, obstacles.maxBy(_._1)._1 + 1),
      (Direction.LEFT, -1)
    )
    val visited = mutable.Set.empty[(Int, Int)]
    var outOfBounds = false
    while !outOfBounds do
      val nextObstacle = obstacles.filter(sees(guardPos, guardDir)).minByOption(manhattan(guardPos))
      guardDir match
        case Direction.UP    =>
          visited.addAll(for i <- guardPos._1.until(nextObstacle.map(_._1).getOrElse(bounds(guardDir)), -1) yield (i, guardPos._2))
          guardPos = (nextObstacle.map(_._1).getOrElse(bounds(guardDir)) + 1, guardPos._2)
        case Direction.RIGHT =>
          visited.addAll(for j <- guardPos._2 until nextObstacle.map(_._2).getOrElse(bounds(guardDir)) yield (guardPos._1, j))
          guardPos = (guardPos._1, nextObstacle.map(_._2).getOrElse(bounds(guardDir)) - 1)
        case Direction.DOWN  =>
          visited.addAll(for i <- guardPos._1 until nextObstacle.map(_._1).getOrElse(bounds(guardDir)) yield (i, guardPos._2))
          guardPos = (nextObstacle.map(_._1).getOrElse(bounds(guardDir)) - 1, guardPos._2)
        case Direction.LEFT  =>
          visited.addAll(for j <- guardPos._2.until(nextObstacle.map(_._2).getOrElse(bounds(guardDir)), -1) yield (guardPos._1, j))
          guardPos = (guardPos._1, nextObstacle.map(_._2).getOrElse(bounds(guardDir)) + 1)

      guardDir = guardDir.next
      outOfBounds = nextObstacle.isEmpty
    visited.size
  }

  override def solve2(puzzle: Puzzle): Any = {
    var (obstacles0, guardPos0, guardDir0) = puzzle
    val bounds: Map[Direction, Int] = Map(
      (Direction.UP, -1),
      (Direction.RIGHT, obstacles0.maxBy(_._2)._2 + 1),
      (Direction.DOWN, obstacles0.maxBy(_._1)._1 + 1),
      (Direction.LEFT, -1)
    )
    val options = for
      i <- 0 to obstacles0.maxBy(_._1)._1
      j <- 0 to obstacles0.maxBy(_._2)._2
      if (i, j) != guardPos0
    yield {
      val obstacles = obstacles0 + ((i, j))
      val visitedFrom = Map.from(Direction.iterator.map((_, mutable.Set.empty[(Int, Int)])))
      var (guardPos, guardDir) = (guardPos0, guardDir0)
      var inLoop = false
      var outOfBounds = false
      while !inLoop && !outOfBounds do
        val nextObstacle = obstacles.filter(sees(guardPos, guardDir)).minByOption(manhattan(guardPos))
        inLoop = !nextObstacle.forall(visitedFrom(guardDir).add)
        guardPos = guardDir match
          case Direction.UP    =>
            (nextObstacle.map(_._1).getOrElse(bounds(guardDir)) + 1, guardPos._2)
          case Direction.RIGHT =>
            (guardPos._1, nextObstacle.map(_._2).getOrElse(bounds(guardDir)) - 1)
          case Direction.DOWN  =>
            (nextObstacle.map(_._1).getOrElse(bounds(guardDir)) - 1, guardPos._2)
          case Direction.LEFT  =>
            (guardPos._1, nextObstacle.map(_._2).getOrElse(bounds(guardDir)) + 1)
        guardDir = guardDir.next
        outOfBounds = nextObstacle.isEmpty
      inLoop
    }
    options.count(x => x)
  }
}
