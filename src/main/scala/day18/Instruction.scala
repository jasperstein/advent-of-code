package day18

import scala.util.Try

case class State(pc: Int, regs: Map[Char, Long], snd: Long)

sealed trait Instruction {
  def delta(regs:Map[Char, Long]) = 1

  def action(regs: Map[Char, Long]): Map[Char, Long] = regs

  def valOrReg(expr: String, regs: Map[Char, Long]): Long = Try(Integer.parseInt(expr).asInstanceOf[Long]).getOrElse(regs(expr.charAt(0)))

  def apply(s: State): State = State(s.pc + delta(s.regs), action(s.regs), s.snd)
}

case class SetReg(reg: Char, to: String) extends Instruction {
  override def action(regs: Map[Char, Long]): Map[Char, Long] = regs.updated(reg, valOrReg(to, regs))
}

case class Add(reg: Char, to: String) extends Instruction {
  override def action(regs: Map[Char, Long]): Map[Char, Long] = regs.updated(reg, regs(reg) + valOrReg(to, regs))
}

case class Mul(reg: Char, to: String) extends Instruction {
  override def action(regs: Map[Char, Long]): Map[Char, Long] = regs.updated(reg, regs(reg) * valOrReg(to, regs))
}

case class Mod(reg: Char, to: String) extends Instruction {
  override def action(regs: Map[Char, Long]): Map[Char, Long] = regs.updated(reg, regs(reg) % valOrReg(to, regs))
}

case class Snd(freq: String) extends Instruction {
  override def apply(s: State): State = s.copy(s.pc + 1, snd = valOrReg(freq, s.regs))
}

case class Rcv(reg: Char) extends Instruction {
  override def apply(s: State): State = {
    if (s.regs(reg) != 0) println(s.snd)
    s.copy(pc = -1)
  }
}

case class Jgz(reg: String, jmp: String) extends Instruction {
  override def delta(regs: Map[Char, Long]): Int = if (valOrReg(reg, regs) != 0) valOrReg(jmp, regs).asInstanceOf[Int] else 1
}