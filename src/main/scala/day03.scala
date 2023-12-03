package day03

import scala.io.Source

type Grid = Vector[Vector[Char]]

final case class Symbol(value: Char, i: Int, j: Int) {
  def isAdjacentTo(n: Number): Boolean = {
    (i == n.i - 1 || i == n.i || i == n.i + 1) &&
    (n.j.contains(j - 1) || n.j.contains(j) || n.j.contains(j + 1))
  }
}

final case class Number(value: Int, i: Int, j: Range)

def getInput(): Grid = Source.fromResource("03-input.txt").getLines().toVector.map(_.toVector)

def isSymbol(c: Char): Boolean = !c.isDigit && c != '.'

def findSymbols(grid: Grid): Vector[Symbol] = {
  (for {
    i <- grid.indices
    j <- grid(i).indices
    if isSymbol(grid(i)(j))
  } yield Symbol(grid(i)(j), i, j)).toVector
}

def findNumbers(grid: Grid): Vector[Number] = {
  val pat = """(\d+)""".r
  (for {
    i <- grid.indices
    matches = pat.findAllMatchIn(grid(i).mkString)
    m <- matches
  } yield Number(m.toString.toInt, i, m.start until m.end)).toVector
}

@main def part1: Unit = {
  val grid    = getInput()
  val symbols = findSymbols(grid)
  val numbers = findNumbers(grid)
  val adjacents = for {
    s <- symbols
    n <- numbers
    if (s.isAdjacentTo(n))
  } yield n
  println(s"Part 1 Solution: ${adjacents.distinct.map(_.value).sum}")
}

@main def part2: Unit = {
  val grid    = getInput()
  val gears   = findSymbols(grid).filter(_.value == '*')
  val numbers = findNumbers(grid)
  val result = gears.map { g =>
    val adjacents = numbers.filter(g.isAdjacentTo)
    if (adjacents.size == 2) adjacents(0).value * adjacents(1).value
    else 0
  }.sum
  println(s"Part 2 Solution: $result")
}
