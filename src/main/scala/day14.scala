package day14

import scala.annotation.tailrec
import scala.io.Source

import Direction.*
import Tile.*

enum Tile {
  case RoundRock, CubeRock, Empty
}

enum Direction {
  case North, West, South, East
}

type Grid = Vector[Vector[Tile]]

def getInput(): Vector[Vector[Tile]] = {
  Source
    .fromResource("14-input.txt")
    .getLines()
    .map(_.map {
      case 'O' => RoundRock
      case '#' => CubeRock
      case '.' => Empty
    }.toVector)
    .toVector
}

def tilt(grid: Grid, dir: Direction) = {
  val t = dir match {
    case North => grid.transpose
    case West  => grid
    case South => grid.reverse.transpose
    case East  => grid.map(_.reverse)
  }

  val tilted = t.map { row =>
    0.until(row.size).foldLeft(row) { case (acc, i) =>
      acc(i) match {
        case Empty =>
          (i + 1)
            .until(grid.size)
            .find(acc(_) != Empty)
            .map { j =>
              acc(j) match {
                case RoundRock => acc.updated(i, RoundRock).updated(j, Empty)
                case _         => acc
              }
            }
            .getOrElse(acc)
        case _ => acc
      }
    }
  }

  dir match {
    case North => tilted.transpose
    case West  => tilted
    case South => tilted.transpose.reverse
    case East  => tilted.map(_.reverse)
  }
}

def calculateLoad(grid: Grid): Long =
  grid.transpose.map { row =>
    row.zipWithIndex.foldLeft(0L) {
      case (acc, (RoundRock, i)) => acc + (grid.size - i)
      case (acc, _)              => acc
    }
  }.sum

def cycle(grid: Grid): Grid = Direction.values.foldLeft(grid)(tilt(_, _))

def findLoadCycle(grid: Grid): (Int, Int) = {
  @tailrec
  def getMeetingPoint(tortoise: Grid, hare: Grid): (Grid, Grid) = {
    if (tortoise != hare) getMeetingPoint(cycle(tortoise), cycle(cycle(hare)))
    else (tortoise, hare)
  }

  @tailrec
  def getFirstRepetition(tortoise: Grid, hare: Grid, mu: Int = 0): (Grid, Grid, Int) = {
    if (tortoise != hare) getFirstRepetition(cycle(tortoise), cycle(hare), mu + 1)
    else (tortoise, hare, mu)
  }

  @tailrec
  def getLoopLength(tortoise: Grid, hare: Grid, lambda: Int = 1): Int = {
    if (tortoise != hare) getLoopLength(tortoise, cycle(hare), lambda + 1)
    else lambda
  }

  val tortoise    = cycle(grid)
  val hare        = cycle(cycle(grid))
  val (t1, h1)    = getMeetingPoint(tortoise, hare)
  val (t2, _, mu) = getFirstRepetition(grid, h1)
  val lambda      = getLoopLength(t2, cycle(t2))

  (mu, lambda)
}

@main def part1: Unit = {
  val grid   = getInput()
  val result = calculateLoad(tilt(grid, North))

  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val grid         = getInput()
  val (mu, lambda) = findLoadCycle(grid)
  val loadCycle    = Iterator.iterate(grid)(cycle).map(calculateLoad).drop(mu).take(lambda).toVector
  val result       = loadCycle((1_000_000_000 - mu) - (1_000_000_000 - mu) / lambda * lambda)
  println(s"Part 2 Solution: $result")
}
