package day6

object day6 extends App {

  case class Coord(x: Int, y:Int)
  val coords = day6Input.input.lines.map(_.split(Array(',',' '))).map(arr => Coord(Integer.parseInt(arr(0)), Integer.parseInt(arr(2)))).toList
  println(coords)

  val minX = coords.minBy(_.x).x
  val maxX = coords.maxBy(_.x).x
  val minY = coords.minBy(_.y).y
  val maxY = coords.maxBy(_.y).y

  def distance(c1: Coord, c2: Coord) = Math.abs(c2.x - c1.x) + Math.abs(c2.y - c1.y)

  var coordAreas = Map[Coord, Int]().withDefaultValue(0)

  for (i <- minX - (maxX - minX) until maxX + (maxX - minX)) {
    for (j <- minY - (maxY - minY) until maxY + (maxY - minY)) {
      val distances: List[(Int, Coord)] = coords.map(c => distance(c, Coord(i, j))).zip(coords).sortBy(_._1)
//      println(Coord(i,j) -> distances)
      if (distances.head._1 != distances(1)._1) {
        coordAreas = coordAreas + (distances.head._2 -> (coordAreas(distances.head._2) + 1))
      }
    }
  }

//List((Coord(110,280),622), (Coord(326,237),698), (Coord(329,225),761), (Coord(308,202),1015), (Coord(261,146),1018), (Coord(217,300),1180), (Coord(203,289),1186), (Coord(201,139),1330), (Coord(318,293),1338), (Coord(144,273),1482), (Coord(196,163),1485), (Coord(222,125),1534), (Coord(200,312),1584), (Coord(255,151),1742), (Coord(300,249),1794), (Coord(283,130),1810), (Coord(235,265),1816), (Coord(297,187),1860), (Coord(242,250),1987), (Coord(268,225),2064), (Coord(136,210),2109), (Coord(149,291),2265), (Coord(119,214),2337), (Coord(104,71),2391), (Coord(180,190),2745), (Coord(183,242),2754), (Coord(107,123),2812), (Coord(100,151),2967), (Coord(236,88),3248), (Coord(173,144),3290), (Coord(112,275),5769), (Coord(91,300),10642), (Coord(322,291),12724), (Coord(354,201),21312), (Coord(312,70),27504), (Coord(243,328),28329), (Coord(313,124),29604), (Coord(46,189),32681), (Coord(346,188),32907), (Coord(289,331),33264), (Coord(350,245),35305), (Coord(56,209),42589), (Coord(116,49),48251), (Coord(47,117),52417), (Coord(227,47),61943), (Coord(174,356),75007), (Coord(84,60),427126), (Coord(327,317),439634), (Coord(305,50),441188), (Coord(86,304),457987))

//  List((Coord(110,280),622), (Coord(326,237),698), (Coord(329,225),761), (Coord(308,202),1015), (Coord(261,146),1018), (Coord(217,300),1180), (Coord(203,289),1186), (Coord(201,139),1330), (Coord(318,293),1338), (Coord(144,273),1482), (Coord(196,163),1485), (Coord(222,125),1534), (Coord(200,312),1584), (Coord(255,151),1742), (Coord(300,249),1794), (Coord(283,130),1810), (Coord(235,265),1816), (Coord(297,187),1860), (Coord(242,250),1987), (Coord(268,225),2064), (Coord(136,210),2109), (Coord(149,291),2265), (Coord(119,214),2337), (Coord(104,71),2391), (Coord(180,190),2745), (Coord(183,242),2754), (Coord(107,123),2812), (Coord(100,151),2967), (Coord(236,88),3248), (Coord(173,144),3290), (Coord(112,275),3921), (Coord(91,300),6316), (Coord(322,291),6872), (Coord(354,201),10840), (Coord(312,70),15184), (Coord(243,328),15351), (Coord(313,124),16360), (Coord(46,189),16973), (Coord(346,188),17199), (Coord(289,331),17814), (Coord(350,245),18057), (Coord(56,209),22569), (Coord(116,49),24767), (Coord(47,117),27161), (Coord(227,47),31970), (Coord(174,356),38545), (Coord(84,60),119385), (Coord(327,317),125744), (Coord(305,50),126018), (Coord(86,304),135466))

  println(coordAreas.toList.sortBy(_._2))

  var withinRange = 0
  for (i <- minX - (10000 / coords.size) until maxX + (10000 / coords.size)) {
    for (j <- minY - (10000 / coords.size) until maxY + (10000 / coords.size)) {
      if (coords.map(c => distance(c, Coord(i,j))).sum < 10000) withinRange = withinRange + 1
    }
  }
  println(withinRange)
}
