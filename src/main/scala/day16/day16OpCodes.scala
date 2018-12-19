package day16

object day16OpCodes {

  sealed trait OpCode {
    def exec(rs: Map[Int, Int]): Map[Int, Int]
  }

  case class Addr(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (rs(a) + rs(b)))
  }
  case class Addi(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (rs(a) + b))
  }
  case class Mulr(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (rs(a) * rs(b)))
  }
  case class Muli(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (rs(a) * b))
  }
  case class Banr(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (rs(a) ^ rs(b)))
  }
  case class Bani(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (rs(a) ^ b))
  }
  case class Borr(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (rs(a) | rs(b)))
  }
  case class Bori(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (rs(a) | b))
  }
  case class Setr(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> rs(a))
  }
  case class Seti(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> a)
  }
  case class Gtir(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (if (a>rs(b)) 1 else 0))
  }
  case class Gtri(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (if (rs(a)>b) 1 else 0))
  }
  case class Gtrr(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (if (rs(a)>rs(b)) 1 else 0))
  }
  case class Eqir(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (if (a==rs(b)) 1 else 0))
  }
  case class Eqri(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (if (rs(a)==b) 1 else 0))
  }
  case class Eqrr(a: Int, b: Int, c: Int) extends OpCode {
    override def exec(rs: Map[Int, Int]): Map[Int, Int] = rs + (c -> (if (rs(a)==rs(b)) 1 else 0))
  }

  def parseOpCode(s:String): (Int, Int, Int) => OpCode = s match {
    case "addr" => Addr.apply
    case "addi" => Addi.apply
    case "mulr" => Mulr.apply
    case "muli" => Muli.apply
    case "banr" => Banr.apply
    case "bani" => Bani.apply
    case "borr" => Borr.apply
    case "bori" => Bori.apply
    case "setr" => Setr.apply
    case "seti" => Seti.apply
    case "gtir" => Gtir.apply
    case "gtri" => Gtri.apply
    case "gtrr" => Gtrr.apply
    case "eqir" => Eqir.apply
    case "eqri" => Eqri.apply
    case "eqrr" => Eqrr.apply
  }


}
