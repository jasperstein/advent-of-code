package day13

import scala.util.Try

object day13 extends App {

  val input = day13Input.input.lines.map(l => (l + (" " * 50)).take(151)).toList

  case class Coord(x: Int, y: Int)

  sealed trait Direction {
    def move(c: Coord): Coord
  }

  case object N extends Direction {
    override def move(c: Coord): Coord = Coord(c.x, c.y - 1)
  }

  case object S extends Direction {
    override def move(c: Coord): Coord = Coord(c.x, c.y + 1)
  }

  case object E extends Direction {
    override def move(c: Coord): Coord = Coord(c.x + 1, c.y)
  }

  case object W extends Direction {
    override def move(c: Coord): Coord = Coord(c.x - 1, c.y)
  }

  def turnCart(c: Cart): Unit = {
    val p = c.position
    val d = c.direction
    c.direction = input(p.y).charAt(p.x) match {
      case '/' => d match {
        case N => E
        case S => W
        case E => N
        case W => S
      }
      case '\\' => d match {
        case N => W
        case S => E
        case E => S
        case W => N
      }
      case '+' =>
        c.turnIntersection()
      case ' ' => throw new Exception
      case _ => d
    }
  }

  sealed trait Turn

  case object Left extends Turn

  case object Straight extends Turn

  case object Right extends Turn

  case class Cart(
              var position: Coord,
              var direction: Direction,
              var turn: Turn = Left,
              var collided: Boolean = false
            ) {
    def move(): Unit = if (!collided) {
      position = direction.move(position)
      turnCart(this)
    }

    def turnIntersection(): Direction = {
      val newDirection = turn match {
        case Straight => direction
        case Left => direction match {
          case N => W
          case S => E
          case E => N
          case W => S
        }
        case Right => direction match {
          case N => E
          case S => W
          case E => S
          case W => N
        }
      }

      turn = turn match {
        case Left => Straight
        case Straight => Right
        case Right => Left
      }

      newDirection
    }
  }

  var carts: List[Cart] = (for (
      (line, lineNr) <- input.zipWithIndex;
      (c, charNr) <- line.zipWithIndex
      if "<>^v".contains(c)
    ) yield {
      Cart(Coord(charNr, lineNr), directionFromChar(c))
    }).sortBy(c => 200 * c.position.y + c.position.x)

//  println(carts)

  class CrashException(val c: Cart) extends Exception

  def tick(): Unit = {
    carts = carts.sortBy(c => 200 * c.position.y + c.position.x)
    for (cart <- carts) {
//      if (ticks > 5793 && !cart.collided) print(cart)
      cart.move()
//      if (ticks > 5793 && !cart.collided) println(cart)
      if (!cart.collided && carts.count(c => c.position == cart.position && !c.collided) > 1) {
        carts.filter(_.position == cart.position).foreach(c => {
          println(c)
          c.collided = true
        })
      }
    }
  }

  def directionFromChar(c: Char): Direction = c match {
    case '<' => W
    case '>' => E
    case '^' => N
    case 'v' => S
  }

  def printBoard(): Unit = {
    for (y <- input.indices) {
      for (x <- 0 until input(y).length) {
        carts.find(c => c.position == Coord(x, y)) match {
          case None => print(input(y).charAt(x))
          case Some(c) => print(if (c.collided) '@' else c.direction)
        }
      }
      println()
    }
  }

  var ticks = 0
  println(carts.count(!_.collided))

  while (carts.count(!_.collided) > 1) {
//    println(carts.count(!_.collided))
    if (List(5795, 5796).contains(ticks)) println(carts.sortBy(c => 200 * c.position.y + c.position.x))
    tick()
    ticks = ticks + 1
    if (List(5794, 5795, 5796).contains(ticks)) printBoard()
    println(ticks)
  }
  println(carts)
}
