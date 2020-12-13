import scala.reflect.io.File

object Day12 extends App {
  private val input: List[String] = File("Inputs/day12.txt").lines().toList

  val example =
    """F10
N3
F7
R90
F11""".split("\n").map(_.trim).toList

  trait Position {
    val x, y: Int
    val manhattan: Int = x.abs + y.abs
  }


  trait Movable[T] {
    val move: T => (Int, Int) => T
  }

  trait Turnable[T] {
    val turnLeft: T => T
    def turn(n: Int, t: T): T = if (n == 0) t else turn(n - 1, turnLeft(t))
  }

  trait Forwardable[T] {
    val forward: T => Int => T
  }


  sealed abstract class Command(letter: Char) {
    def parseNum(s: String): Int = s.toInt
    def unapply(s: String): Option[Int] = if (s.startsWith(letter.toString)) Some(parseNum(s.tail)) else None
  }

  sealed abstract class Orientation(letter: Char) extends Command(letter) {
    def move[T : Movable](n: Int, curr: T): T
  }

  case object N extends Orientation('N') {
    override def move[T: Movable](n: Int, curr: T): T = implicitly[Movable[T]].move(curr)(0, n)
  }

  case object S extends Orientation('S') {
    override def move[T: Movable](n: Int, curr: T): T = N.move(-n, curr)
  }

  case object E extends Orientation('E') {
    override def move[T: Movable](n: Int, curr: T): T = implicitly[Movable[T]].move(curr)(n, 0)
  }

  case object W extends Orientation('W') {
    override def move[T: Movable](n: Int, curr: T): T = E.move(-n, curr)
  }


  sealed abstract class Turn(letter: Char) extends Command(letter) {
    def numLeftTurns(degrees: Int): Int
    override def parseNum(s: String): Int = numLeftTurns(s.toInt)
    def turn[T : Turnable](i: Int, curr: T): T = implicitly[Turnable[T]].turn(i, curr)
  }

  case object L extends Turn('L') {
    override def numLeftTurns(degrees: Int): Int = degrees / 90
  }

  case object R extends Turn('R') {
    override def numLeftTurns(degrees: Int): Int = (360 - degrees) / 90
  }


  case object F extends Command('F') {
    def forward[T : Forwardable](i: Int, curr: T): T = implicitly[Forwardable[T]].forward(curr)(i)
  }


  private def stateChange[T: Movable : Turnable : Forwardable](curr: T, instr: String): T =
    instr match {
      case N(n) => N.move(n, curr)
      case E(n) => E.move(n, curr)
      case S(n) => S.move(n, curr)
      case W(n) => W.move(n, curr)

      case L(n) => L.turn(n, curr)
      case R(n) => R.turn(n, curr)

      case F(n) => F.forward(n, curr)

      case _ => ???
    }

  private def finalPosition[T: Movable : Turnable : Forwardable](startState: T, route: List[String]): T =
    route.foldLeft(startState)(stateChange)



  case class State(x: Int, y: Int, o: Orientation) extends Position
  val startState = State(0, 0, E)

  implicit val movableState: Movable[State] = new Movable[State] {
    override val move: State => (Int, Int) => State =
      curr => (dx,dy) => curr.copy(x = curr.x + dx, y = curr.y + dy)
  }

  implicit val turnableState: Turnable[State] = new Turnable[State] {
    override val turnLeft: State => State =
      curr => curr.copy(o = Map[Orientation, Orientation](N -> W, W -> S, S -> E, E -> N)(curr.o))
  }

  implicit val forwardableState: Forwardable[State] = new Forwardable[State] {
    override val forward: State => Int => State =
      curr => n => curr.o.move(n, curr)
  }

  println(finalPosition(startState, example))
  println(finalPosition(startState, example).manhattan)
  println(finalPosition(startState, input))
  println(finalPosition(startState, input).manhattan)



  case class StateW(x: Int, y: Int, wx: Int, wy: Int) extends Position
  val startStateW = StateW(0,0, 10, 1)

  implicit val movableW: Movable[StateW] = new Movable[StateW] {
    override val move: StateW => (Int, Int) => StateW =
      curr => (dx, dy) => curr.copy(wx = curr.wx + dx, wy = curr.wy + dy)
  }

  implicit val turnableW: Turnable[StateW] = new Turnable[StateW] {
    override val turnLeft: StateW => StateW = t => t.copy(wx = -t.wy, wy = t.wx)
  }

  implicit val forwardableW: Forwardable[StateW] = new Forwardable[StateW] {
    override val forward: StateW => Int => StateW =
      curr => n => curr.copy(x = curr.x + n * curr.wx, y = curr.y + n * curr.wy)
  }

  println(finalPosition(startStateW, example))
  println(finalPosition(startStateW, example).manhattan)
  println(finalPosition(startStateW, input))
  println(finalPosition(startStateW, input).manhattan)

}
