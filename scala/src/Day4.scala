import scala.reflect.io.File

object Day4 extends App {
  private val input: List[String] = File("Inputs/day4.txt").lines().toList

  case class PassportSpec(fields: List[(String, String)]) {
    private val keys = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

    lazy val hasEnoughFields = fields.toMap.keySet.intersect(keys.toSet).size == 7

    lazy val isValid = hasEnoughFields && keys.forall(k => requirements(k)(fields.toMap.apply(k)))

    def dump(): Unit = {
      if (hasEnoughFields) {
        println(fields)
        keys.foreach(k => println(k + fields.toMap.apply(k) + requirements(k)(fields.toMap.apply(k))))
      }
    }
  }

  /*
  byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
*/

  val requirements: Map[String, String => Boolean] = Map(
    "byr" -> { v => v.matches("\\d{4}") && (1920 to 2002).contains(Integer.parseInt(v)) },
    "iyr" -> { v => v.matches("\\d{4}") && (2010 to 2020).contains(Integer.parseInt(v)) },
    "eyr" -> { v => v.matches("\\d{4}") && (2020 to 2030).contains(Integer.parseInt(v)) },
    "hgt" -> { v => v.matches("\\d+(cm|in)") && (if (v.endsWith("cm")) (150 to 193) else (59 to 76)).contains(Integer.parseInt(v.dropRight(2)))},
    "hcl" -> { v => v.matches("#[0-9a-f]{6}")},
    "ecl" -> { v => "amb blu brn gry grn hzl oth".split(" ").contains(v) },
    "pid" -> { v => v.matches("\\d{9}") },
    "cid" -> { _ => true}
  )

  def fields(line: String): List[(String, String)] =
    line.split("\\s+").toList.map(item => item.split(':')).map(kvs => kvs(0) -> kvs(1))

  private val specs: (PassportSpec, List[PassportSpec]) =
    input.foldLeft((PassportSpec(List()), List[PassportSpec]()))({
      case ((curr, all), line) =>
        if (line.trim.isBlank) PassportSpec(List()) -> (all :+ curr)
        else PassportSpec(curr.fields ++ fields(line)) -> all
    })

  println(specs._2.count(_.hasEnoughFields))
  specs._2.foreach(_.dump())
  println(specs._2.count(_.isValid))
}
