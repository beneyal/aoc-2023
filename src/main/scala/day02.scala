package day02

import scala.io.Source
import Color.*

enum Color {
  case Red, Green, Blue
}

final case class Cube(color: Color, amount: Int)
final case class Game(id: Int, cubes: Vector[Vector[Cube]])

def getInput() = {
  Source.fromResource("02-input.txt").getLines().toVector.map { case s"Game $id: $cubesInfo" =>
    val cubes = cubesInfo
      .split("; ")
      .map(
        _.split(", ")
          .map {
            case s"$amount red"   => Cube(Red, amount.toInt)
            case s"$amount green" => Cube(Green, amount.toInt)
            case s"$amount blue"  => Cube(Blue, amount.toInt)
          }
          .toVector
      )
      .toVector
    Game(id.toInt, cubes)
  }
}

def getMaxAmountOfEachColor(cubes: Vector[Vector[Cube]]): (Int, Int, Int) = {
  cubes.flatten.foldLeft((0, 0, 0)) { case ((r, g, b), cube) =>
    cube.color match {
      case Red   => (r.max(cube.amount), g, b)
      case Green => (r, g.max(cube.amount), b)
      case Blue  => (r, g, b.max(cube.amount))
    }
  }
}

def isPossible(cubes: Vector[Vector[Cube]]): Boolean = {
  val (r, g, b) = getMaxAmountOfEachColor(cubes)
  r <= 12 && g <= 13 && b <= 14
}

@main def part1: Unit = {
  val input    = getInput()
  val possible = input.collect { case Game(id, cubes) if isPossible(cubes) => id }.sum
  println(s"Part 1 Solution: $possible")
}

@main def part2: Unit = {
  val input = getInput()
  val power = input.map { game =>
    val (r, g, b) = getMaxAmountOfEachColor(game.cubes)
    r * g * b
  }.sum
  println(s"Part 2 Solution: $power")
}
