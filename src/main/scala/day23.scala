package day23

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
}

type Grid          = Vector[Vector[Char]]
type WeightedGraph = Map[Position, Vector[(Position, Int)]]

object WeightedGraph {
  def empty: WeightedGraph = Map.empty.withDefaultValue(Vector.empty)
}

extension (grid: Grid) {
  def apply(pos: Position) = grid(pos._1)(pos._2)
}

def getInput(): Grid = {
  Source.fromResource("23-input.txt").getLines().toVector.map(_.toVector)
}

def buildGraph(grid: Grid, withSlopes: Boolean): WeightedGraph = {
  val n = grid.size

  (for {
    i <- 0 until n
    j <- 0 until n
    pos = (i, j)
  } yield
    if (withSlopes)
      grid(pos) match {
        case '.' => pos -> pos.neighbors.filter(_.inBounds(n)).filter(grid(_) != '#').map(_ -> 1)
        case '^' => pos -> Vector(((i - 1, j), 1))
        case '>' => pos -> Vector(((i, j + 1), 1))
        case 'v' => pos -> Vector(((i + 1, j), 1))
        case '<' => pos -> Vector(((i, j - 1), 1))
        case '#' => pos -> Vector.empty
      }
    else
      grid(pos) match {
        case '#' => pos -> Vector.empty
        case _   => pos -> pos.neighbors.filter(_.inBounds(n)).filter(grid(_) != '#').map(_ -> 1)
      }).toMap
}

def minimizeGraph(graph: WeightedGraph, source: Position, target: Position): WeightedGraph = {
  val minimizationPoints = graph.keySet.filter(graph(_).size > 2) ++ Set(source, target)

  def loop(
      source: Position,
      stack: List[(Position, Int)],
      seen: Set[Position],
      result: WeightedGraph
  ): WeightedGraph = {
    stack match {
      case Nil => result
      case (v, vw) :: rest =>
        val neighbors = graph(v)
        if (vw != 0 && minimizationPoints(v)) {
          loop(source, rest, seen, result.updated(source, result(source) :+ (v, vw)))
        } else {
          val (newStack, newSeen) =
            neighbors.map(_._1).filterNot(seen).foldLeft((rest, seen)) { case ((stack, seen), u) =>
              ((u, vw + 1) :: stack, seen + u)
            }
          loop(source, newStack, newSeen, result)
        }
    }
  }

  minimizationPoints.foldLeft(WeightedGraph.empty) { case (acc, v) =>
    loop(v, List((v, 0)), Set(v), acc)
  }
}

def hike(graph: WeightedGraph, source: Position, target: Position): Int = {
  def recur(v: Position, seen: Set[Position]): Int = {
    if (v == target) 0
    else
      graph(v)
        .filterNot { case (u, _) => seen(u) }
        .map { case (u, uw) => recur(u, seen + v) + uw }
        .maxOption
        .getOrElse(Int.MinValue)
  }

  recur(source, Set.empty)
}

def solve(withSlopes: Boolean): Int = {
  val grid           = getInput()
  val source         = (0, 1)
  val target         = (grid.size - 1, grid.size - 2)
  val graph          = buildGraph(grid, withSlopes)
  val minimizedGraph = minimizeGraph(graph, source, target)
  hike(minimizedGraph, source, target)
}

@main def part1(): Unit = {
  val result = solve(withSlopes = true)
  println(s"Part 1 Solution: $result")
}

@main def part2(): Unit = {
  val result = solve(withSlopes = false)
  println(s"Part 2 Solution: $result")
}
