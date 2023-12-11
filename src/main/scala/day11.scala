package day11

import scala.io.Source

final case class Point(i: Int, j: Int)
final case class Void(is: Set[Int], js: Set[Int])

type Universe = Vector[Vector[Char]]

extension (universe: Universe) {
  def theVoid: Void = {
    val is = universe.zipWithIndex.collect { case (row, i) if row.forall(_ == '.') => i }
    val js = universe.transpose.zipWithIndex.collect { case (col, j) if col.forall(_ == '.') => j }
    Void(is.toSet, js.toSet)
  }

  def galaxies: Vector[Point] = {
    (for {
      i <- universe.indices
      j <- universe(i).indices
      if (universe(i)(j) == '#')
    } yield Point(i, j)).toVector
  }
}

def getInput() = {
  Source.fromResource("11-input.txt").getLines().toVector.map(_.toVector)
}

def manhattanDistanceInSpace(g1: Point, g2: Point, void: Void, expansionFactor: Int): Long = {
  val iAxis  = (g1.i - g2.i).abs
  val jAxis  = (g1.j - g2.j).abs
  val iRange = if (g1.i < g2.i) (g1.i until g2.i) else (g2.i until g1.i)
  val jRange = if (g1.j < g2.j) (g1.j until g2.j) else (g2.j until g1.j)
  iAxis + jAxis + (void.is.count(iRange.contains) + void.js.count(jRange.contains)) * (expansionFactor - 1)
}

def solve(expansionFactor: Int): Long = {
  val universe = getInput()
  val void     = universe.theVoid
  val pairs    = universe.galaxies.combinations(2)
  pairs.map(gs => manhattanDistanceInSpace(gs(0), gs(1), void, expansionFactor)).sum
}

@main def part1: Unit = {
  val result = solve(2)
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val result = solve(1_000_000)
  println(s"Part 2 Solution: $result")
}
