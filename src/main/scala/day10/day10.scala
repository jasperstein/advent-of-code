package day10

object day10 extends App {

  case class Coord(x: Int, y: Int, vx: Int, vy: Int)

  //position=<-50429,  40580> velocity=< 5, -4>

  def parseCoords(s: String): Coord = {
    val x = Integer.parseInt(s.substring(10, 16).trim)
    val y = Integer.parseInt(s.substring(17, 24).trim)
    val vx = Integer.parseInt(s.substring(36, 38).trim)
    val vy = Integer.parseInt(s.substring(39, 42).trim)
    Coord(x,y, vx,vy)
  }

//  parseCoords("position=<-50429,  40580> velocity=< 5, -4>")

  val input = day10Input.input.lines.map(parseCoords).toList

//  println(input)

  def iterate(coord: Coord): Coord = coord match {
    case Coord(x, y, vx, vy) => Coord(x+vx, y+vy, vx, vy)
  }

  def height(coords: List[Coord]): Int = coords.maxBy(_.y).y - coords.minBy(_.y).y

  var stars = input
  var i = 0
  while (height(stars) > 10 && i < 100000) {
    stars = stars.map(iterate)
    i = i + 1
  }

  println(stars.minBy(_.x).x, stars.maxBy(_.x).x)
  println(stars.minBy(_.y).y, stars.maxBy(_.y).y)

  for (y <- 109 to 118) {
    for (x <- 140 to 201) {
      if (stars.exists(star => star.x == x && star.y == y)) print('#') else print('.')
    }
    println()
  }
  println(i)
}
