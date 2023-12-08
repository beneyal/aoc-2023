package day08

import scala.annotation.tailrec
import scala.io.Source

import Direction.*

enum Direction {
  case Left, Right
}

def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

def lcm(a: Long, b: Long): Long = (a * b).abs / gcd(a, b)

final case class DesertMap(directions: LazyList[Direction], map: Map[String, (String, String)]) {
  def stepsUntilEnd(start: String, isEnd: String => Boolean): Long = {
    @tailrec
    def loop(node: String, directions: LazyList[Direction], result: Long): Long = {
      if (isEnd(node)) result
      else {
        val (left, right) = map(node)
        directions.head match {
          case Left  => loop(left, directions.tail, result + 1)
          case Right => loop(right, directions.tail, result + 1)
        }
      }
    }

    loop(start, directions, 0L)
  }

  def stepsUntilEndForGhosts: Long =
    map.keys.filter(_.endsWith("A")).map(stepsUntilEnd(_, _.endsWith("Z"))).foldLeft(1L) { case (acc, x) =>
      lcm(acc, x)
    }
}

def getInput(): DesertMap = {
  val chunks = Source.fromResource("08-input.txt").getLines().mkString("\n").split("\n\n")
  val dirs = LazyList
    .continually(chunks(0).map {
      case 'L' => Left
      case 'R' => Right
    })
    .flatten
  val map =
    chunks(1).split("\n").foldLeft(Map.empty[String, (String, String)]) { case (acc, s"$node = ($left, $right)") =>
      acc.updated(node, (left, right))
    }
  DesertMap(dirs, map)
}

@main def part1: Unit = {
  val result = getInput().stepsUntilEnd("AAA", _ == "ZZZ")
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val result = getInput().stepsUntilEndForGhosts
  println(s"Part 2 Solution: $result")
}
