import scala.io.Source

object day01 {
  def getInput(): Vector[String] =
    Source.fromResource("01-input.txt").getLines().toVector

  def part1(input: Vector[String]): Int =
    input.map { line =>
      val digits = line.filter(_.isDigit).toVector
      s"${digits.head}${digits.last}".toInt
    }.sum

  def part2(input: Vector[String]): Int = {
    val pat = """(?=(\d|one|two|three|four|five|six|seven|eight|nine))""".r
    input.map { line =>
      val digits = pat
        .findAllMatchIn(line)
        .map(_.group(1))
        .map {
          case "one"   => "1"
          case "two"   => "2"
          case "three" => "3"
          case "four"  => "4"
          case "five"  => "5"
          case "six"   => "6"
          case "seven" => "7"
          case "eight" => "8"
          case "nine"  => "9"
          case d       => d
        }
        .toVector

      s"${digits.head}${digits.last}".toInt
    }.sum
  }

  @main def main: Unit = {
    val input = getInput()
    println(s"Part 1 Solution: ${part1(input)}")
    println(s"Part 2 Solution: ${part2(input)}")
  }
}
