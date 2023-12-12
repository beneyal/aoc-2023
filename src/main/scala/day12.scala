package day12

import scala.collection.mutable
import scala.io.Source

import Condition.*

enum Condition {
  case Operational, Damaged, Unknown
}

final case class Row(conditions: List[Condition], groups: List[Int]) {
  def expand: Row = Row(List.fill(5)(conditions).flatMap(_ :+ Unknown).init, List.fill(5)(groups).flatten)
}

def getInput(): Vector[Row] = {
  Source
    .fromResource("12-input.txt")
    .getLines()
    .map { case s"$conditions $groups" =>
      Row(
        conditions.map {
          case '.' => Operational
          case '#' => Damaged
          case '?' => Unknown
        }.toList,
        groups.split(",").map(_.toInt).toList
      )
    }
    .toVector
}

def countArrangements(row: Row): Long = {
  val cache = mutable.Map.empty[(Int, Int), Long]

  def canBeDamaged(c: Condition): Boolean     = c != Operational
  def canBeOperational(c: Condition): Boolean = c != Damaged

  def recur(i: Int, groups: List[Int]): Long = {
    val key = (i, groups.size)

    if (i == row.conditions.size)
      if (groups.isEmpty) 1L else 0L
    else {
      cache.getOrElseUpdate(
        key, {
          val c             = row.conditions(i)
          val ifOperational = if (canBeOperational(c)) recur(i + 1, groups) else 0L
          val ifDamaged = groups match {
            case g :: gs if i + g <= row.conditions.size =>
              val slice = row.conditions.slice(i, i + g)
              if (slice.forall(canBeDamaged)) {
                if (i + g == row.conditions.size) recur(i + g, gs)
                else if (canBeOperational(row.conditions(i + g))) recur(i + g + 1, gs)
                else 0L
              } else {
                0L
              }
            case _ =>
              0L
          }
          ifOperational + ifDamaged
        }
      )
    }
  }

  recur(0, row.groups)
}

@main def part1: Unit = {
  val result = getInput().map(countArrangements).sum
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val result = getInput().map(_.expand).map(countArrangements).sum
  println(s"Part 2 Solution: $result")
}
