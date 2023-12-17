package day17

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source

import Direction.*

type Grid = Vector[Vector[Int]]

enum Direction {
  case Up, Down, Left, Right
}

type Position = (Int, Int)

extension (p: Position) {
  def move(dir: Direction): Position = {
    val (i, j) = p
    dir match {
      case Up    => (i - 1, j)
      case Down  => (i + 1, j)
      case Left  => (i, j - 1)
      case Right => (i, j + 1)
    }
  }

  def up: Position    = move(Up)
  def down: Position  = move(Down)
  def left: Position  = move(Left)
  def right: Position = move(Right)

  def inBounds(n: Int): Boolean = {
    val (i, j) = p
    0.until(n).contains(i) && 0.until(n).contains(j)
  }
}

final case class QueueState(pos: Position, dir: Direction, steps: Int)

final case class WeightedEdge[A](destination: A, weight: Long)

type WeightedGraph[A] = Map[A, List[WeightedEdge[A]]]

object WeightedGraph {
  def empty[A]: WeightedGraph[A] = Map.empty
}

def getInput(): Grid = {
  Source.fromResource("17-input.txt").getLines().map(_.map(_.asDigit).toVector).toVector
}

def solve(grid: Grid, minBound: Int, maxBound: Int): Long = {
  val n     = grid.size
  val range = minBound.until(maxBound)

  def updateGraph(qs: QueueState, graph: WeightedGraph[QueueState], qss: List[QueueState]): WeightedGraph[QueueState] =
    graph.updated(qs, qss.map { case qs @ QueueState((i, j), _, _) => WeightedEdge(qs, grid(i)(j)) })

  def loop(q: Queue[QueueState], seen: Set[QueueState], graph: WeightedGraph[QueueState]): WeightedGraph[QueueState] = {
    if (q.isEmpty) graph
    else {
      val (qs @ QueueState(pos, dir, steps), q0) = q.dequeue
      if (seen(qs)) loop(q0, seen, graph)
      else
        dir match {
          case Up if steps == maxBound =>
            val qss = List(QueueState(pos.right, Right, 1), QueueState(pos.left, Left, 1)).filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Up if range.contains(steps) =>
            val qss =
              List(QueueState(pos.right, Right, 1), QueueState(pos.left, Left, 1), QueueState(pos.up, Up, steps + 1))
                .filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Up =>
            val qss = List(QueueState(pos.up, Up, steps + 1)).filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Down if steps == maxBound =>
            val qss =
              List(QueueState(pos.right, Right, 1), QueueState(pos.left, Left, 1)).filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Down if range.contains(steps) =>
            val qss = List(
              QueueState(pos.down, Down, steps + 1),
              QueueState(pos.right, Right, 1),
              QueueState(pos.left, Left, 1)
            )
              .filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Down =>
            val qss = List(QueueState(pos.down, Down, steps + 1)).filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Left if steps == maxBound =>
            val qss =
              List(QueueState(pos.down, Down, 1), QueueState(pos.up, Up, 1)).filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Left if range.contains(steps) =>
            val qss =
              List(QueueState(pos.left, Left, steps + 1), QueueState(pos.down, Down, 1), QueueState(pos.up, Up, 1))
                .filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Left =>
            val qss = List(QueueState(pos.left, Left, steps + 1)).filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Right if steps == maxBound =>
            val qss = List(QueueState(pos.down, Down, 1), QueueState(pos.up, Up, 1)).filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Right if range.contains(steps) =>
            val qss =
              List(QueueState(pos.right, Right, steps + 1), QueueState(pos.down, Down, 1), QueueState(pos.up, Up, 1))
                .filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
          case Right =>
            val qss = List(QueueState(pos.right, Right, steps + 1)).filter(_.pos.inBounds(n))
            loop(q0.enqueueAll(qss), seen + qs, updateGraph(qs, graph, qss))
        }
    }
  }

  val graph = loop(Queue(QueueState(0 -> 0, Right, 0), QueueState(0 -> 0, Down, 0)), Set.empty, WeightedGraph.empty)

  List(Right, Down)
    .map(d => shortestPaths(graph, QueueState((0, 0), d, 0)).filter(_._1.pos == (n - 1, n - 1)).map(_._2).min)
    .min
}

def shortestPaths[A](graph: WeightedGraph[A], source: A): Map[A, Long] = {
  val distances = mutable.Map.empty[A, Long]
  val pq        = mutable.PriorityQueue(source -> 0L)(using Ordering.by(-_._2))

  while (pq.nonEmpty) {
    val (u, dist) = pq.dequeue()
    if (!distances.contains(u)) {
      distances(u) = dist
      graph(u).foreach { case WeightedEdge(v, w) =>
        if (!distances.contains(v)) {
          pq.enqueue(v -> (dist + w))
        }
      }
    }
  }

  distances.toMap
}

@main def part1: Unit = {
  val grid   = getInput()
  val result = solve(grid, 0, 3)
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val grid   = getInput()
  val result = solve(grid, 4, 10)
  println(s"Part 2 Solution: $result")
}
