package day17

object Spinlock extends App {

  val star1 = 376

  val buffer = new Array[Int](2018)
  buffer(0) = 0
  var pos = 0
  for (i <- 1 until 2018) {
    pos = ((pos + star1) % i) + 1
    for (j <- Range(i, pos, -1)) {
      buffer(j) = buffer(j - 1)
    }
    buffer(pos) = i
  }
  println(buffer.toList)
}
