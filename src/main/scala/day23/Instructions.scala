package day23

import scala.util.Try

case class State(pc: Int, regs: Map[Char, Long], muls: Long)

sealed trait Instruction {
  def delta(regs: Map[Char, Long]) = 1

  def action(regs: Map[Char, Long]): Map[Char, Long] = regs

  def valOrReg(expr: String, regs: Map[Char, Long]): Long = Try(Integer.parseInt(expr).asInstanceOf[Long]).getOrElse(regs(expr.charAt(0)))

  def apply(s: State): State = State(s.pc + delta(s.regs), action(s.regs), s.muls)
}

case class SetReg(reg: Char, to: String) extends Instruction {
  override def action(regs: Map[Char, Long]): Map[Char, Long] = regs.updated(reg, valOrReg(to, regs))
}

case class Sub(reg: Char, to: String) extends Instruction {
  override def action(regs: Map[Char, Long]): Map[Char, Long] = regs.updated(reg, regs(reg) - valOrReg(to, regs))
}

case class Mul(reg: Char, to: String) extends Instruction {
  override def action(regs: Map[Char, Long]): Map[Char, Long] = regs.updated(reg, regs(reg) * valOrReg(to, regs))

  override def apply(s: State): State = State(s.pc + 1, action(s.regs), s.muls + 1)
}

case class Jnz(reg: String, jmp: String) extends Instruction {
  override def delta(regs: Map[Char, Long]): Int = if (valOrReg(reg, regs) != 0) valOrReg(jmp, regs).asInstanceOf[Int] else 1
}

