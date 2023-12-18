package day18

import scala.io.Source

import Direction.*

enum Direction {
  case Up, Down, Left, Right
}

type Position = (Int, Int)

extension (p: Position) {
  def move(dir: Direction, meters: Int): Position = {
    val (i, j) = p
    dir match {
      case Up    => (i - meters, j)
      case Down  => (i + meters, j)
      case Left  => (i, j - meters)
      case Right => (i, j + meters)
    }
  }
}

final case class Instruction(dir: Direction, meters: Int, color: String) {
  def getCorrect: Instruction = {
    val meters = Integer.parseInt(color.take(5), 16)
    val dir = color.last match {
      case '0' => Right
      case '1' => Down
      case '2' => Left
      case '3' => Up
    }
    Instruction(dir, meters, color)
  }
}

def getInput(): Vector[Instruction] = {
  Source
    .fromResource("18-input.txt")
    .getLines()
    .map { case s"$dir $m (#$color)" =>
      dir match {
        case "U" => Instruction(Up, m.toInt, color)
        case "D" => Instruction(Down, m.toInt, color)
        case "L" => Instruction(Left, m.toInt, color)
        case "R" => Instruction(Right, m.toInt, color)
      }
    }
    .toVector
}

def dig(instructions: Vector[Instruction]): (Vector[Position], Long) = {
  val result = instructions
    .foldLeft((0 -> 0, Vector.empty[Position], 0L)) { case ((cur, vs, boundaryCount), Instruction(dir, meters, _)) =>
      (cur.move(dir, meters), vs :+ cur, boundaryCount + meters)
    }
  (result._2, result._3)
}

def polygonArea(instructions: Vector[Instruction]): Long = {
  val (vs, boundary) = dig(instructions)
  val xs             = vs.map(_._1.toLong)
  val ys             = vs.map(_._2.toLong)

  val area = xs
    .zip(xs.tail :+ xs.head)
    .zip(ys.zip(ys.tail :+ ys.head))
    .map { case ((xk, xk1), (yk, yk1)) =>
      (xk + xk1) * (yk - yk1)
    }
    .sum / 2

  boundary + (area - boundary / 2 + 1)
}

@main def part1: Unit = {
  val result = polygonArea(getInput())
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val result = polygonArea(getInput().map(_.getCorrect))
  println(s"Part 2 Solution: $result")
}
