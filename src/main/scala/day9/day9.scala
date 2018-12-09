package day9

object day9 extends App {

  val input = day9Input.input

  val players = 476
  val marbles = 7143100

  var scores = Map[Int, Long]().withDefaultValue(0L)

  var circle = Array(0)

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

  def placeMarble(circle: Array[Int], nextMarble: Int, currIdx: Int): (Int, Int, Array[Int]) = {
    if ((nextMarble % 23) != 0) {
      val toTake = (currIdx + 1) % circle.length
      (0, toTake + 1, (circle.take(toTake + 1) :+ nextMarble) ++ circle.drop(toTake + 1))
    } else {
      val delIdx = (currIdx + circle.length - 7) % circle.length
      val score = nextMarble + circle(delIdx)
      (score, delIdx, circle.take(delIdx) ++ circle.drop(delIdx + 1))
    }
  }

  def place22Marbles(circle: Array[Int], nextMarble: Int, currIdx: Int): Array[Int] = {
    (circle.take(currIdx + 2) ++ Array(
      (nextMarble + 0) , circle(currIdx + 2)
      , (nextMarble + 1) , circle(currIdx + 3)
      , (nextMarble + 2) , circle(currIdx + 4)
      , (nextMarble + 3) , circle(currIdx + 5)
      , (nextMarble + 4) , circle(currIdx + 6)
      , (nextMarble + 5) , circle(currIdx + 7)
      , (nextMarble + 6) , circle(currIdx + 8)
      , (nextMarble + 7) , circle(currIdx + 9)
      , (nextMarble + 8) , circle(currIdx + 10)
      , (nextMarble + 9) , circle(currIdx + 11)
      , (nextMarble + 10) , circle(currIdx + 12)
      , (nextMarble + 11) , circle(currIdx + 13)
      , (nextMarble + 12) , circle(currIdx + 14)
      , (nextMarble + 13) , circle(currIdx + 15)
      , (nextMarble + 14) , circle(currIdx + 16)
      , (nextMarble + 15) , circle(currIdx + 17)
      , (nextMarble + 16) , circle(currIdx + 18)
      , (nextMarble + 17) , circle(currIdx + 19)
      , (nextMarble + 18) , circle(currIdx + 20)
      , (nextMarble + 19) , circle(currIdx + 21)
      , (nextMarble + 20) , circle(currIdx + 22)
      , (nextMarble + 21)) ++ circle.drop(currIdx + 23))
  }

//  println(place22Marbles(Array(0,1,2,3,4,5,6,7,8), 9, 1).toList)
  var marble = 1
  while (marble <= marbles) {
    if (marble % 23 == 1 && currIndex < circle.length - 23 && marble < marbles - 25) {
//      println(circle.toList)
//      println(currIndex)
//      println(marble)
      val nextCircle = place22Marbles(circle, marble, currIndex)
      currIndex = currIndex + 44
      circle = nextCircle
      marble = marble + 22
//      println(circle.toList)
//      println(currIndex)
//      println(marble)
//      throw new Exception {}
    } else {
      val (score, nextIndex, nextcircle) = placeMarble(circle, marble, currIndex)
      currIndex = nextIndex
      circle = nextcircle
      //    println(circle)
      //    println(currIndex)
      val l: Long = scores(marble % players) + score.toLong
      scores = scores + ((marble % players) -> l)
      //    println(scores)
      if (marble % 10000 == 0) println(marble)
      marble = marble + 1
    }
  }
  println(scores.toList.sortBy(_._2))

//-1228659943
//3066307353
//  println(Int.MaxValue-1228659943)
}
