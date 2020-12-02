import scala.reflect.io.File

object Day2 extends App {

  case class PwSpec(min: Int, max: Int, ch: Char, pwd: String) {
    val isValid = (min to max) contains pwd.count(_ == ch)
    val isValid2 = pwd.charAt(min - 1) == ch ^ pwd.charAt(max - 1) == ch
  }

  val regex = "(\\d+)-(\\d+)\\s*(\\w):\\s*(\\w*)".r.pattern

  private val parseEntry: String => PwSpec = entry => {
    val matcher = regex.matcher(entry)
    matcher.matches()
    PwSpec(
      Integer.parseInt(matcher.group(1)),
      Integer.parseInt(matcher.group(2)),
      matcher.group(3).charAt(0),
      matcher.group(4))
  }

  private val input: List[PwSpec] = File("Inputs/day2.txt").lines().map(parseEntry).toList

  println(input.count(_.isValid))
  println(input.count(_.isValid2))
}
