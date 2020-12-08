import scala.reflect.io.File

object Day8 extends App {
    private val input: List[String] = File("Inputs/day8.txt").lines().toList

    private val program = input.map(_.split(" ")).map(parts => parts.head -> Integer.parseInt(parts(1)))

    case class Trace(visited: List[Int], acc: Int, pc: Int)

    def step(program: List[(String,  Int)], trace: Trace): Either[Int, Trace] = {
        import trace._
        if (pc < program.length) {
            val inst = program(pc)
            Right(inst._1 match {
                case "nop" => Trace(visited :+ pc, acc, pc + 1)
                case "jmp" => Trace(visited :+ pc, acc, pc + inst._2)
                case "acc" => Trace(visited :+ pc, acc + inst._2, pc + 1)
            })
        } else Left(trace.acc)
    }

    def tracer(prg: List[(String, Int)]): Seq[Either[Int, Trace]] = Seq.unfold[Either[Int, Trace], Either[Int, Trace]](Right(Trace(List(), 0, 0))) {
        case Right(trace) =>
            import trace._
            if (visited.contains(pc)) None
            else Some(step(prg, trace), step(prg, trace))
        case Left(_) => None
    }

    private val part1: Either[Int, Trace] = tracer(program).last
    println(part1)

    private val tweakedPrograms: List[List[(String, Int)]] = part1.getOrElse(???).visited.reverse.map(i => program.updated(i, program(i) match {
        case ("jmp", i) => ("nop", i)
        case ("nop", i) => ("jmp", i)
        case ("acc", i) => ("acc", i)
    }))

    private val finalProgram: List[(String, Int)] = tweakedPrograms.find(tracer(_).last.isLeft).getOrElse(???)
    println(tracer(finalProgram).last)
}
