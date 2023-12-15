package day15

import scala.io.Source

import Instruction.*

enum Instruction {
  case Remove
  case Insert(focalLength: Long)
}

final case class Operation(label: String, instruction: Instruction) {
  def hash(whole: Boolean = false): Int = {
    val s =
      if (whole)
        instruction match {
          case Remove              => s"${label}-"
          case Insert(focalLength) => s"${label}=${focalLength}"
        }
      else label
    s.foldLeft(0) { case (acc, c) =>
      (acc + c.toInt) * 17 % 256
    }
  }
}

type HashMap = Vector[Vector[(String, Long)]]

object HashMap {
  def empty: Vector[Vector[(String, Long)]] = Vector.fill(256)(Vector.empty)
}

def getInput(): Vector[Operation] = {
  Source.fromResource("15-input.txt").getLines().next().split(",").toVector.map {
    case s"${label}-"               => Operation(label, Remove)
    case s"${label}=${focalLength}" => Operation(label, Insert(focalLength.toLong))
  }
}

def getFocusingPower(ops: Vector[Operation]): Long = {
  val map = ops.foldLeft(HashMap.empty) { case (acc, op @ Operation(label, ins)) =>
    val hash = op.hash()
    val vec  = acc(hash)
    val idx  = vec.indexWhere(_._1 == label)
    ins match {
      case Remove =>
        if (idx >= 0) acc.updated(hash, vec.patch(idx, Vector.empty, 1))
        else acc
      case Insert(focalLength) =>
        val value = label -> focalLength
        if (idx >= 0) acc.updated(hash, vec.updated(idx, value))
        else acc.updated(hash, vec :+ value)
    }
  }

  map.zipWithIndex
    .filter(_._1.nonEmpty)
    .map { case (box, bi) =>
      box.zipWithIndex.map { case ((_, focalLength), li) => (bi + 1) * (li + 1) * focalLength }.sum
    }
    .sum
}

@main def part1: Unit = {
  val result = getInput().map(_.hash(whole = true)).sum
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val result = getFocusingPower(getInput())
  println(s"Part 2 Solution: $result")
}
