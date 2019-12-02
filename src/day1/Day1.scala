package day1

import java.nio.file.{Files, Path}
import collection.convert.AsScalaConverters

object Day1 extends App with AsScalaConverters {
  def fuelForMass(mass: Int): Int = Math.max((mass / 3) - 2, 0)

  private val modules = asScala(Files.readAllLines(Path.of("src/day1/Input.txt"))).map(Integer.parseInt)
  val modulesFuel = modules.map(fuelForMass).sum
  println(modulesFuel)

  def totalFuelForMass(mass: Int): Int = {
    var fuel = fuelForMass(mass)
    var totalFuel = 0
    while (fuel > 0) {
      totalFuel = totalFuel + fuel
      println(s"Added $fuel to get $totalFuel")
      fuel = fuelForMass(fuel)
    }
    totalFuel
  }
  val totalFuel = modules.map(totalFuelForMass).sum
  println(totalFuel)
}
