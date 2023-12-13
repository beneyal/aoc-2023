package day13

import scala.io.Source

type Pattern = Vector[Vector[Char]]

def getInput(): Vector[Pattern] = {
  Source
    .fromResource("13-input.txt")
    .getLines()
    .mkString("\n")
    .split("\n\n")
    .map(_.split("\n").map(_.toVector).toVector)
    .toVector
}

def findReflectionPoint(pattern: Pattern): Option[Int] = {
  1.until(pattern.size).find { i =>
    val (left, right) = pattern.splitAt(i)
    left.reverse.zip(right).forall(_ == _)
  }
}

def findReflectionPointWithSmudge(pattern: Pattern): Option[Int] = {
  1.until(pattern.size).find { i =>
    val (left, right) = pattern.splitAt(i)
    val smudges       = left.reverse.zip(right).map { case (l, r) => l.zip(r).count(_ != _) }.sum
    smudges == 1
  }
}

@main def part1: Unit = {
  val result = getInput().flatMap { pat =>
    findReflectionPoint(pat).map(_ * 100).orElse(findReflectionPoint(pat.transpose))
  }.sum
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val result = getInput().flatMap { pat =>
    findReflectionPointWithSmudge(pat).map(_ * 100).orElse(findReflectionPointWithSmudge(pat.transpose))
  }.sum
  println(s"Part 2 Solution: $result")
}
