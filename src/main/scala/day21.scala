package day21

import scala.io.Source

type Position = (Int, Int)

extension (p: Position) {
  def neighbors: Vector[Position] = {
    val (i, j) = p
    Vector((i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1))
  }

  def inBounds(n: Int): Boolean = {
    val (i, j) = p
    0.until(n).contains(i) && 0.until(n).contains(j)
  }

  def wrapAround(n: Int): Position = {
    val (i, j) = p
    val iMod   = i % n
    val jMod   = j % n
    (if (iMod < 0) iMod + n else iMod, if (jMod < 0) jMod + n else jMod)
  }
}

extension (grid: Grid) {
  def apply(p: Position): Char = {
    val (i, j) = p.wrapAround(grid.size)
    val c      = grid(i)(j)
    if (c == 'S') '.' else c
  }
}

type Grid = Vector[Vector[Char]]

final case class FiniteGridState(grid: Grid, seen: Set[Position]) {
  def nextState: FiniteGridState =
    copy(seen = seen.flatMap(_.neighbors).filter(_.inBounds(grid.size)).filter(grid(_) != '#'))
}

final case class InfiniteGridState(grid: Grid, index: Int, seen: Set[Position]) {
  def nextState: InfiniteGridState = copy(
    index = index + 1,
    seen = seen.flatMap(_.neighbors).filter(grid(_) != '#')
  )
}

def getInput() = {
  val grid = Source.fromResource("21-input.txt").getLines().toVector.map(_.toVector)
  val start = (for {
    i <- grid.indices
    j <- grid(i).indices
    if (grid(i)(j)) == 'S'
  } yield (i, j)).head

  (grid, start)
}

@main def part1: Unit = {
  val (grid, start) = getInput()
  val result        = Iterator.iterate(FiniteGridState(grid, Set(start)))(_.nextState).drop(64).next.seen.size
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val (grid, start @ (mid, _)) = getInput()
  val side                     = grid.size
  val xs                       = Set(mid, mid + side, mid + 2 * side)
  val Vector(f0, f1, f2) = Iterator
    .iterate(InfiniteGridState(grid, 0, Set(start)))(_.nextState)
    .filter(s => xs(s.index))
    .take(3)
    .map(_.seen.size)
    .toVector

  val a = (f2 - 2 * f1 + f0) / 2
  val b = f1 - f0 - a
  val c = f0
  val f = (x: Long) => a * x * x + b * x + c

  val result = f((26_501_365 - mid) / side)

  println(s"Part 2 Solution: $result")
}
