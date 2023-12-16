package day16

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

import Direction.*

type Cave     = Vector[Vector[Char]]
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

  def inBounds(n: Int): Boolean = {
    val (i, j) = p
    0.until(n).contains(i) && 0.until(n).contains(j)
  }
}

enum Direction {
  case Up, Down, Left, Right
}

def getInput() = {
  Source.fromResource("16-input.txt").getLines().toVector.map(_.toVector)
}

def reflect(cave: Cave, source: (Position, Direction)) = {
  @tailrec
  def loop(q: Queue[(Position, Direction)], seen: Set[(Position, Direction)]): Int = {
    if (q.isEmpty) seen.map(_._1).size
    else {
      val (x @ (p @ (i, j), dir), q0) = q.dequeue
      if (!p.inBounds(cave.size) || seen(x)) loop(q0, seen)
      else
        cave(i)(j) match {
          case '.' => loop(q0.enqueue(p.move(dir) -> dir), seen + x)
          case '|' =>
            dir match {
              case Up | Down    => loop(q0.enqueue(p.move(dir) -> dir), seen + x)
              case Left | Right => loop(q0.enqueueAll(List(p.move(Up) -> Up, p.move(Down) -> Down)), seen + x)
            }
          case '-' =>
            dir match {
              case Up | Down    => loop(q0.enqueueAll(List(p.move(Left) -> Left, p.move(Right) -> Right)), seen + x)
              case Left | Right => loop(q0.enqueue(p.move(dir) -> dir), seen + x)
            }
          case '/' =>
            dir match {
              case Up    => loop(q0.enqueue(p.move(Right) -> Right), seen + x)
              case Down  => loop(q0.enqueue(p.move(Left) -> Left), seen + x)
              case Left  => loop(q0.enqueue(p.move(Down) -> Down), seen + x)
              case Right => loop(q0.enqueue(p.move(Up) -> Up), seen + x)
            }
          case '\\' =>
            dir match {
              case Up    => loop(q0.enqueue(p.move(Left) -> Left), seen + x)
              case Down  => loop(q0.enqueue(p.move(Right) -> Right), seen + x)
              case Left  => loop(q0.enqueue(p.move(Up) -> Up), seen + x)
              case Right => loop(q0.enqueue(p.move(Down) -> Down), seen + x)
            }
        }
    }
  }

  loop(Queue(source), Set.empty)
}

@main def part1: Unit = {
  val result = reflect(getInput(), (0, 0) -> Right)
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val cave = getInput()
  val result =
    cave.indices
      .flatMap(i => List((0, i) -> Down, (cave.size - 1, i) -> Up, (i, 0) -> Right, (i, cave.size - 1) -> Left))
      .map(reflect(cave, _))
      .max
  println(s"Part 2 Solution: $result")
}
