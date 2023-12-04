package day04

import scala.io.Source

final case class Card(id: Int, winningNums: Set[Int], numsYouHave: Set[Int]) {
  def points: Int = math.pow(2.0, (winningNums & numsYouHave).size - 1).toInt
}

def getInput() = {
  val CardPat = """Card\s+(\d+):\s+(.*)\|\s+(.*)""".r
  Source.fromResource("04-input.txt").getLines().toVector.map { case CardPat(cardNum, winningNums, numsYouHave) =>
    Card(
      cardNum.toInt,
      winningNums.split("""\s+""").map(_.toInt).toSet,
      numsYouHave.split("""\s+""").map(_.toInt).toSet
    )
  }
}

@main def part1: Unit = {
  val cards = getInput()
  println(s"Part 1 Solution: ${cards.map(_.points).sum}")
}

@main def part2: Unit = {
  val cards  = getInput()
  val counts = cards.map(c => c.id -> 1).toMap
  val id2count = cards.foldLeft(counts) { case (acc, card) =>
    val currentCount = acc(card.id)
    val matches      = (card.winningNums & card.numsYouHave).size
    val newCounts    = (card.id + 1 to card.id + matches).map(id => id -> (acc(id) + currentCount)).toMap
    acc ++ newCounts
  }
  println(s"Part 2 Solution: ${id2count.values.sum}")
}
