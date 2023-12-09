package day09

import scala.annotation.tailrec
import scala.io.Source

type History = Vector[Long]
type Diffs   = Vector[Vector[Long]]

def getInput(): Vector[History] = {
  Source.fromResource("09-input.txt").getLines().map(_.split(" ").map(_.toLong).toVector).toVector
}

def getDiffs(history: History) = {
  @tailrec
  def loop(history: History, result: Diffs): Diffs =
    if (history.forall(_ == 0)) result
    else {
      val diffs = history.sliding(2).collect { case Vector(x, y) => y - x }.toVector
      loop(diffs, result :+ diffs)
    }

  loop(history, Vector(history))
}

def extrapolate(diffs: Diffs, f: (Long, Vector[Long]) => Long): Long = {
  diffs.reverse.tail.foldLeft(0L)(f)
}

@main def part1: Unit = {
  val result = getInput().map(getDiffs).map(diffs => extrapolate(diffs, _ + _.last)).sum
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val result = getInput().map(getDiffs).map(diffs => extrapolate(diffs, -_ + _.head)).sum
  println(s"Part 2 Solution: $result")
}
