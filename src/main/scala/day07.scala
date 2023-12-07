package day07

import scala.io.Source

import HandType.*

type Card = Char
type Hand = Vector[Card]

enum HandType {
  case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
}

extension (hand: Hand) {
  def getType: HandType = {
    val counts = hand.groupBy(identity).view.mapValues(_.size).values.toList.sorted.reverse
    counts match {
      case 5 :: Nil      => FiveOfAKind
      case 4 :: _        => FourOfAKind
      case 3 :: 2 :: Nil => FullHouse
      case 3 :: _        => ThreeOfAKind
      case 2 :: 2 :: _   => TwoPair
      case 2 :: _        => OnePair
      case _             => HighCard
    }
  }

  def getTypeWithJoker: HandType = {
    val counts           = hand.groupBy(identity).view.mapValues(_.size).toMap
    val numJokers        = counts.get('J').getOrElse(0)
    val countsMinusJoker = counts.removed('J')
    val valuesMinusJoker = countsMinusJoker.values.toList.sorted.reverse
    val cardValues =
      if (numJokers == 0) valuesMinusJoker
      else
        valuesMinusJoker match {
          case Nil => List(numJokers)
          case _   => valuesMinusJoker.head + numJokers :: valuesMinusJoker.tail
        }
    cardValues match {
      case 5 :: Nil      => FiveOfAKind
      case 4 :: _        => FourOfAKind
      case 3 :: 2 :: Nil => FullHouse
      case 3 :: _        => ThreeOfAKind
      case 2 :: 2 :: _   => TwoPair
      case 2 :: _        => OnePair
      case _             => HighCard
    }
  }
}

given Ordering[HandType] = Ordering.fromLessThan(_.ordinal < _.ordinal)

def handCompare(x: Hand, y: Hand, getType: Hand => HandType)(using
    handTypeOrd: Ordering[HandType],
    cardOrd: Ordering[Card]
): Int = {
  val comp = handTypeOrd.compare(getType(x), getType(y))
  if (comp == 0) {
    x.zip(y).find(_ != _).map { case (c1, c2) => cardOrd.compare(c1, c2) }.getOrElse(0)
  } else {
    comp
  }
}

def getInput(): Vector[(Hand, Long)] = {
  Source.fromResource("07-input.txt").getLines().toVector.map { case s"$hand $bid" =>
    (hand.toVector, bid.toLong)
  }
}

@main def part1: Unit = {
  given handOrdering(using handTypeOrd: Ordering[HandType], cardOrd: Ordering[Card]): Ordering[Hand] with {
    override def compare(x: Hand, y: Hand): Int = handCompare(x, y, _.getType)
  }

  val ranks            = "23456789TJQKA"
  given Ordering[Card] = Ordering.by(ranks.indexOf(_))

  val (hands, bids) = getInput().sortBy(_._1).unzip
  val result        = hands.zipWithIndex.map { case (_, i) => (i + 1) * bids(i) }.sum
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  given handOrdering(using handTypeOrd: Ordering[HandType], cardOrd: Ordering[Card]): Ordering[Hand] with {
    override def compare(x: Hand, y: Hand): Int = handCompare(x, y, _.getTypeWithJoker)
  }

  val ranks            = "J23456789TQKA"
  given Ordering[Card] = Ordering.by(ranks.indexOf(_))

  val (hands, bids) = getInput().sortBy(_._1).unzip
  val result        = hands.zipWithIndex.map { case (_, i) => (i + 1) * bids(i) }.sum
  println(s"Part 2 Solution: $result")
}
