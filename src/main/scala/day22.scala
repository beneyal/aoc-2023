package day22

import scala.annotation.tailrec
import scala.io.Source

type Cube = (Int, Int, Int)

extension (cube: Cube) {
  def lower: Cube = (cube._1, cube._2, cube._3 - 1)
}

type Brick = Vector[Cube]

final case class FallState(seen: Set[Cube], bricks: Vector[Brick], stop: Boolean)

def getInput() = {
  Source
    .fromResource("22-input.txt")
    .getLines()
    .map { case s"$x0,$y0,$z0~$x1,$y1,$z1" =>
      val xStart = x0.toInt
      val xEnd   = x1.toInt
      val yStart = y0.toInt
      val yEnd   = y1.toInt
      val zStart = z0.toInt
      val zEnd   = z1.toInt

      if (xStart != xEnd) xStart.to(xEnd).map((_, yStart, zStart)).toVector
      else if (yStart != yEnd) yStart.to(yEnd).map((xStart, _, zStart)).toVector
      else if (zStart != zEnd) zStart.to(zEnd).map((xStart, yStart, _)).toVector
      else Vector((xStart, yStart, zStart))

    }
    .toVector
}

def simulateFall(bricks: Vector[Brick]) = {
  @tailrec
  def loop(bricks: Vector[Brick], seen: Set[Cube]): FallState = {
    val initialState = FallState(seen, bricks, true)
    val finalState = bricks.zipWithIndex.foldLeft(initialState) { case (acc, (brick, i)) =>
      val continue = brick.forall { case (x, y, z) =>
        if (z == 1) false
        else if (acc.seen((x, y, z - 1)) && !brick.contains((x, y, z - 1))) false
        else true
      }

      if (continue) {
        val newSeen   = brick.foldLeft(acc.seen) { case (seen, cube) => seen.excl(cube).incl(cube.lower) }
        val newBricks = acc.bricks.updated(i, acc.bricks(i).map(_.lower))
        acc.copy(seen = newSeen, bricks = newBricks, stop = false)
      } else acc
    }

    if (finalState.stop) finalState
    else loop(finalState.bricks, finalState.seen)
  }

  val finalState = loop(bricks, bricks.flatten.toSet)
  (finalState.bricks, finalState.seen)
}

def disintegrate(bricks: Vector[Brick], seen: Set[Cube]) = {
  @tailrec
  def loop(bricks: Vector[Brick], seen: Set[Cube], fell: Set[Int] = Set.empty): Set[Int] = {
    val initialState = FallState(seen, bricks, true)
    val (finalState, finalFell) = bricks.zipWithIndex.foldLeft((initialState, fell)) { case ((acc, fell), (brick, i)) =>
      val continue = brick.forall { case (x, y, z) =>
        if (z == 1) false
        else if (acc.seen((x, y, z - 1)) && !brick.contains((x, y, z - 1))) false
        else true
      }

      if (continue) {
        val newFell   = fell.incl(i)
        val newSeen   = brick.foldLeft(acc.seen) { case (seen, cube) => seen.excl(cube).incl(cube.lower) }
        val newBricks = acc.bricks.updated(i, acc.bricks(i).map(_.lower))
        (acc.copy(seen = newSeen, bricks = newBricks, stop = false), newFell)
      } else (acc, fell)
    }

    if (finalState.stop) finalFell
    else loop(finalState.bricks, finalState.seen, finalFell)
  }

  bricks.indices.map { i =>
    val seen0 = bricks(i).foldLeft(seen)(_.excl(_))
    loop(bricks.patch(i, Vector.empty, 1), seen0)
  }
}

def solve: Vector[Set[Int]] = {
  val snapshot       = getInput()
  val (bricks, seen) = simulateFall(snapshot)
  disintegrate(bricks, seen).toVector
}

@main def part1: Unit = {
  val result = solve.count(_.isEmpty)
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val result = solve.map(_.size).sum
  println(s"Part 2 Solution: $result")
}
