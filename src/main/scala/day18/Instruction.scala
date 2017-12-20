package day18

import scala.util.Try

case class State(pc: Int, regs: Map[Char, Long], snd: Long)

case class State2(pc: Int, regs: Map[Char, Long], sent: Int, queue: List[Long])

case class DuetState(s1: State2, s2: State2)

sealed trait Instruction {
  def delta(regs:Map[Char, Long]) = 1

  def action(regs: Map[Char, Long]): Map[Char, Long] = regs

  def valOrReg(expr: String, regs: Map[Char, Long]): Long = Try(Integer.parseInt(expr).asInstanceOf[Long]).getOrElse(regs(expr.charAt(0)))

  def apply(s: State): State = State(s.pc + delta(s.regs), action(s.regs), s.snd)

  def applyNew(s: State2): State2 = State2(s.pc + delta(s.regs), action(s.regs), s.sent, s.queue)

  def apply1(d: DuetState): DuetState = DuetState(applyNew(d.s1), d.s2)

  def apply2(d: DuetState): DuetState = DuetState(d.s1, applyNew(d.s2))
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

  override def apply1(d: DuetState): DuetState = DuetState(d.s1.copy(pc = d.s1.pc + 1, sent = d.s1.sent + 1), d.s2.copy(queue = valOrReg(freq, d.s1.regs) :: d.s2.queue))
  override def apply2(d: DuetState): DuetState = DuetState(d.s1.copy(queue = valOrReg(freq, d.s2.regs) :: d.s1.queue), d.s2.copy(pc = d.s2.pc + 1, sent = d.s2.sent + 1))
}

case class Rcv(reg: Char) extends Instruction {
  override def apply(s: State): State = {
    if (s.regs(reg) != 0) println(s.snd)
    s.copy(pc = -1)
  }

  override def apply1(d: DuetState): DuetState = if (d.s1.queue.isEmpty) d else {
    val newRegs = d.s1.regs.updated(reg, d.s1.queue.last)
    DuetState(d.s1.copy(d.s1.pc + 1, newRegs, queue = d.s1.queue.init), d.s2)
  }
  override def apply2(d: DuetState): DuetState = if (d.s2.queue.isEmpty) d else {
    val newRegs = d.s2.regs.updated(reg, d.s2.queue.last)
    DuetState(d.s1, d.s2.copy(d.s2.pc + 1, newRegs, queue = d.s2.queue.init))
  }}

case class Jgz(reg: String, jmp: String) extends Instruction {
  override def delta(regs: Map[Char, Long]): Int = if (valOrReg(reg, regs) > 0) valOrReg(jmp, regs).asInstanceOf[Int] else 1
}