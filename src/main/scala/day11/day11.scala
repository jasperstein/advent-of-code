package day11

import scala.collection.mutable

object day11 extends App {

  val serialNo = 3999

  case class Coord(x: Int, y: Int) {
    def rackId = x + 10

    def beginPower = rackId * y

    def increasedPower = beginPower + serialNo

    def power = increasedPower * rackId

    def powerString = power.toString

    def hundredsDigit = powerString.charAt(powerString.length - 3)

    lazy val fuel = Integer.parseInt(s"$hundredsDigit") - 5
  }

  var fuels = Map[Coord, Int]()

  for (x <- 1 to 300) {
    for (y <- 1 to 300) {
      fuels = fuels + (Coord(x, y) -> Coord(x, y).fuel)
    }
  }

//  println(fuels)

  var squares = Map[Coord, Int]()

  for (x <- 1 to 298;
        y <- 1 to 298) {
  squares = squares + (Coord(x,y) ->
    (fuels(Coord(x, y)) +
      fuels(Coord(x + 1, y)) +
      fuels(Coord(x + 2, y)) +
      fuels(Coord(x, y + 1)) +
      fuels(Coord(x + 1, y + 1)) +
      fuels(Coord(x + 2, y + 1)) +
      fuels(Coord(x, y + 2)) +
      fuels(Coord(x + 1, y + 2)) +
      fuels(Coord(x + 2, y + 2))))
  }

  println(squares.toList.maxBy(_._2))

  var maxFuels = mutable.Map[(Coord, Int), Int]()
  for (x <- 1 to 300) {
    println(x)
    for (y <- 1 to 300) {
      for (size <- 0 to 300 - Math.max(x, y)) {
        if (size == 0) {
          maxFuels((Coord(x, y), 0)) = fuels(Coord(x, y))
        } else {
          var fuel = maxFuels((Coord(x, y), size - 1)) +
            (for (dx <- 0 until size) yield fuels(Coord(x + dx, y + size))).sum +
            (for (dy <- 0 until size) yield fuels(Coord(x + size, y + dy))).sum +
            fuels(Coord(x + size, y + size))
          maxFuels((Coord(x, y), size)) = fuel
        }
      }
    }
  }
//  println(maxFuels)
  println(maxFuels.toList.maxBy(_._2))

}
