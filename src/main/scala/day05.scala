package day05

import zio.*
import zio.stream.*

import scala.io.Source

final case class Almanac(seeds: Vector[Long], seedToLocation: Long => Long)

def getInput(): Almanac = {
  def parseChunk(chunk: String) = {
    val f = chunk
      .split("\n")
      .tail
      .map { case s"$destStr $srcStr $rangeStr" =>
        val src   = srcStr.toLong
        val dest  = destStr.toLong
        val range = (src until src + rangeStr.toLong)
        val d     = dest - src
        (n: Long) => if (range.contains(n)) Some(n + d) else None
      }
      .foldLeft((_: Long) => Option.empty[Long]) { case (acc, f) =>
        (n: Long) => f(n).orElse(acc(n))
      }

    (n: Long) => f(n).getOrElse(n)
  }

  val chunks         = Source.fromResource("05-input.txt").getLines().mkString("\n").split("\n\n")
  val seeds          = chunks.head.split(": ")(1).split(" ").map(_.toLong).toVector
  val seedToLocation = chunks.tail.map(parseChunk).foldLeft(identity[Long])(_ andThen _)

  Almanac(seeds, seedToLocation)
}

@main def part1: Unit = {
  val almanac   = getInput()
  val locations = almanac.seeds.map(almanac.seedToLocation)
  println(s"Part 1 Solution: ${locations.min}")
}

@main def part2: Unit = {
  val almanac    = getInput()
  val seedGroups = almanac.seeds.grouped(2).toVector
  val zio = ZStream
    .fromIterable(seedGroups)
    .mapZIOPar(8) { sg =>
      ZStream.fromIterable(sg(0) to sg(0) + sg(1)).runFold(Long.MaxValue)(_ min almanac.seedToLocation(_))
    }
    .runFold(Long.MaxValue)(_ min _)

  val result = Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(zio).getOrThrowFiberFailure()
  }

  println(s"Part 2 Solution: $result")
}
