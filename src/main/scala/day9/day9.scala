package day9

object day9 extends App {

  val input = day9Input.input

  val players = 476
  val marbles = 7143100

  var scores = Map[Int, Int]().withDefaultValue(0)

  var circle = List(0)

  var currIndex = 0

  /*
  [-] (0)
[1]  0 (1)
[2]  0 (2) 1
[3]  0  2  1 (3)
[4]  0 (4) 2  1  3
[5]  0  4  2 (5) 1  3
[6]  0  4  2  5  1 (6) 3
[7]  0  4  2  5  1  6  3 (7)
[8]  0 (8) 4  2  5  1  6  3  7
[9]  0  8  4 (9) 2  5  1  6  3  7
[1]  0  8  4  9  2(10) 5  1  6  3  7
[2]  0  8  4  9  2 10  5(11) 1  6  3  7
[3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
[4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
[5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
[6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
[7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
[8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
[9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
[1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
[2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
[3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
[4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
[5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
[6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
[7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15
   */

  def placeMarble(circle: List[Int], nextMarble: Int, currIdx: Int): (Int, Int, List[Int]) = {
    if ((nextMarble % 23) != 0) {
      val toTake = (currIdx + 1) % circle.size
      (0, toTake + 1, (circle.take(toTake + 1) :+ nextMarble) ++ circle.drop(toTake + 1))
    } else {
      val delIdx = (currIdx + circle.size - 7) % circle.size
      val score = nextMarble + circle(delIdx)
      (score, delIdx, circle.take(delIdx) ++ circle.drop(delIdx + 1))
    }
  }

  for (marble <- 1 to marbles) {
    var (score, nextIndex, nextcircle) = placeMarble(circle, marble, currIndex)
    currIndex = nextIndex
    circle = nextcircle
//    println(circle)
//    println(currIndex)
    scores = scores + ((marble % players) -> (scores(marble % players) + score))
//    println(scores)
    if (marble % 10000 == 0) println(marble)
  }
  println(scores.toList.sortBy(_._2))

}
