package day06

import scala.io.Source

final case class Race(time: Long, distance: Long) {
  def winningCount: Long = (0L to time).map(x => -x * (x - time)).count(_ > distance)
}

def getInput() = {
  val input     = Source.fromResource("06-input.txt").getLines().toVector
  val times     = input(0).split("""\s+""").tail.map(_.toInt)
  val distances = input(1).split("""\s+""").tail.map(_.toInt)
  times.zip(distances).map { case (t, d) => Race(t, d) }.toVector
}

@main def part1: Unit = {
  val result = getInput().map(_.winningCount).product
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val race = getInput().reduce { case (Race(t1, d1), Race(t2, d2)) =>
    Race(s"$t1$t2".toLong, s"$d1$d2".toLong)
  }
  val result = race.winningCount
  println(s"Part 2 Solution: $result")
}
