package day10

import scala.io.Source
import scala.annotation.tailrec

import Tile.*
import scala.collection.immutable.Queue

enum Tile {
  case Vertical, Horizontal, NorthEast, NorthWest, SouthEast, SouthWest, Ground
}

final case class Point(i: Int, j: Int) {
  def neighbors: List[Point] =
    List(Point(i - 1, j), Point(i, j + 1), Point(i + 1, j), Point(i, j - 1))

  def neighborsVia(tile: Tile): List[Point] =
    tile match {
      case Vertical   => List(Point(i - 1, j), Point(i + 1, j))
      case Horizontal => List(Point(i, j - 1), Point(i, j + 1))
      case NorthEast  => List(Point(i - 1, j), Point(i, j + 1))
      case NorthWest  => List(Point(i - 1, j), Point(i, j - 1))
      case SouthEast  => List(Point(i, j + 1), Point(i + 1, j))
      case SouthWest  => List(Point(i, j - 1), Point(i + 1, j))
      case Ground     => List.empty
    }
}

type Field = Vector[Vector[Tile]]
type Graph = Map[Point, Tile]

final case class PipeField(field: Field, source: Point, graph: Graph) {
  def bfs: Set[Point] = {
    @tailrec
    def loop(q: Queue[Point], seen: Set[Point]): Set[Point] = {
      if (q.isEmpty) seen
      else {
        val (p, q0) = q.dequeue
        if (seen(p)) loop(q0, seen)
        else {
          val pipe = graph.get(p)
          val ns   = pipe.map(p.neighborsVia(_)).getOrElse(List.empty)
          loop(q0.enqueueAll(ns), seen + p)
        }
      }
    }

    loop(Queue(source), Set.empty)
  }
}

def getInput() = {
  val rawField = Source.fromResource("10-input.txt").getLines().toVector

  val source = (for {
    i <- rawField.indices
    j <- rawField(i).indices
    if (rawField(i)(j) == 'S')
  } yield Point(i, j)).head

  val graph = rawField.zipWithIndex.foldLeft(Map.empty[Point, Tile]) { case (acc, (row, i)) =>
    row.zipWithIndex.foldLeft(acc) { case (acc, (tile, j)) =>
      val p = Point(i, j)
      tile match {
        case '.' => acc
        case 'S' =>
          val ns = p.neighbors
          val neighborsOfS = for {
            tile     <- Tile.values.toList
            neighbor <- ns
          } yield (tile, neighbor.neighborsVia(tile))
          val s = neighborsOfS.find { case (_, ps) => ps.contains(p) }.map(_._1).get
          acc.updated(p, s)
        case '|' => acc.updated(p, Vertical)
        case '-' => acc.updated(p, Horizontal)
        case 'L' => acc.updated(p, NorthEast)
        case 'J' => acc.updated(p, NorthWest)
        case 'F' => acc.updated(p, SouthEast)
        case '7' => acc.updated(p, SouthWest)
      }
    }
  }

  val field = rawField.indices.toVector.map { i =>
    rawField(i).indices.toVector.map { j =>
      val p = Point(i, j)
      graph.get(p).getOrElse(Ground)
    }
  }

  PipeField(field, source, graph)
}

def rayCast(field: Field, loop: Set[Point], i: Int, j: Int) = {
  (0 until j)
    .filter(k => loop(Point(i, k)))
    .count(k => Set(NorthWest, NorthEast, Vertical).contains(field(i)(k)))
}

@main def part1: Unit = {
  val pipeField = getInput()
  val result    = pipeField.bfs.size / 2
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val pipeField = getInput()
  val field     = pipeField.field
  val loop      = pipeField.bfs
  val result = (for {
    i <- field.indices
    j <- field(i).indices
    p = Point(i, j)
    if (!loop.contains(p))
  } yield rayCast(field, loop, i, j)).filter(_ % 2 != 0).size
  println(s"Part 2 Solution: $result")
}
