package day16

sealed trait Move {
  def dance(lineup: List[Char]): List[Char]
}

case class Spin(n: Int) extends Move {
  def dance(lineup: List[Char]): List[Char] = {
    println(s"spin $n on $lineup")
    lineup.splitAt(lineup.length - n) match {
      case ((l1, l2)) => l2 ++ l1
    }
  }
}

case class Exchange(i1: Int, i2: Int) extends Move {
  override def dance(lineup: List[Char]): List[Char] = {
    println(s"Exch $i1 and $i2 on $lineup")
    lineup.updated(i1, lineup(i2)).updated(i2, lineup(i1))
  }
}

case class Partner(p1: Char, p2: Char) extends Move {
  override def dance(lineup: List[Char]): List[Char] = {
    println(s"Partner $p1 and $p2 on $lineup")
    lineup.updated(lineup.indexOf(p1), p2).updated(lineup.indexOf(p2), p1)
  }
}
