package day20

import scala.collection.immutable.Queue
import scala.io.Source

import Pulse.*

enum ModuleType {
  case Broadcaster, FlipFlop, Conjunction, DeadEnd
}

enum Pulse {
  case Low, High
}

final case class QueueState(origin: String, target: String, kind: ModuleType, pulse: Pulse)

def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

def lcm(a: Long, b: Long): Long = (a * b).abs / gcd(a, b)

def getInput() = {
  val (sources, targets) = Source
    .fromResource("20-input.txt")
    .getLines()
    .map { case s"$from -> $to" => from -> to.split(", ").toVector }
    .toVector
    .unzip

  val typedSources = sources.map {
    case "broadcaster" => ("broadcaster", ModuleType.Broadcaster)
    case s"%$m"        => (m, ModuleType.FlipFlop)
    case s"&$m"        => (m, ModuleType.Conjunction)
  }

  val flipFlops = typedSources.collect { case (name, ModuleType.FlipFlop) => name }

  val conjunctions = typedSources
    .zip(targets)
    .foldLeft(Map.empty[String, Set[String]].withDefaultValue(Set.empty)) { case (acc, ((s, _), ts)) =>
      ts.foldLeft(acc) { case (acc, t) =>
        typedSources.toMap.get(t).collect { case ModuleType.Conjunction => acc.updated(t, acc(t) + s) }.getOrElse(acc)
      }
    }
    .toMap

  val modules = typedSources.toMap

  val graph =
    typedSources.zip(targets).foldLeft(Map.empty[String, Vector[(String, ModuleType)]]) { case (acc, ((s, mt), ts)) =>
      acc.updated(s, ts.map(t => t -> modules.get(t).getOrElse(ModuleType.DeadEnd)))
    }

  val rxOrigin = graph.toList.flatMap { case (s, ts) => ts.find(_._1 == "rx").map(_ => s) }.head

  (graph, flipFlops, conjunctions, rxOrigin)
}

def pushButton(
    graph: Map[String, Vector[(String, ModuleType)]],
    flipFlops: Vector[String],
    conjunctions: Map[String, Set[String]],
    count: Int = 1
) = {
  def loop(
      q: Queue[QueueState],
      flipFlopsState: Map[String, Boolean],
      conjunctionsState: Map[String, Map[String, Pulse]],
      pulseCount: Map[Pulse, Long]
  ): (Map[String, Boolean], Map[String, Map[String, Pulse]], Map[Pulse, Long]) = {
    if (q.isEmpty) (flipFlopsState, conjunctionsState, pulseCount)
    else {
      val (QueueState(origin, target, kind, pulse), q0) = q.dequeue
      val newPulseCount                                 = pulseCount.updatedWith(pulse)(_.map(_ + 1))
      val newTargets                                    = graph.get(target).getOrElse(Vector.empty)
      kind match {
        case ModuleType.Broadcaster => ???
        case ModuleType.FlipFlop =>
          pulse match {
            case Low =>
              val newFlipFlopsState = flipFlopsState.updatedWith(target)(_.map(!_))
              val newPulse          = if (newFlipFlopsState(target)) High else Low
              val q1 = q0.enqueueAll(newTargets.map { case (t, k) => QueueState(target, t, k, newPulse) })
              loop(q1, newFlipFlopsState, conjunctionsState, newPulseCount)
            case High => loop(q0, flipFlopsState, conjunctionsState, newPulseCount)
          }
        case ModuleType.Conjunction =>
          val newConjunctionsState = conjunctionsState.updatedWith(target)(_.map(_.updated(origin, pulse)))
          val newPulse             = if (newConjunctionsState(target).values.forall(_ == High)) Low else High
          val q1                   = q0.enqueueAll(newTargets.map { case (t, k) => QueueState(target, t, k, newPulse) })
          loop(q1, flipFlopsState, newConjunctionsState, newPulseCount)
        case ModuleType.DeadEnd => loop(q0, flipFlopsState, conjunctionsState, newPulseCount)
      }
    }
  }

  val q = Queue(graph("broadcaster").map { case (target, kind) => QueueState("broadcaster", target, kind, Low) }*)
  val flipFlopsState    = flipFlops.map(_ -> false).toMap
  val conjunctionsState = conjunctions.map { case (name, inputs) => name -> (inputs.map(_ -> Low)).toMap }
  val pulseCount        = Map(Low -> 0L, High -> 0L)

  1.to(count)
    .foldLeft((flipFlopsState, conjunctionsState, pulseCount)) { case ((flipFlops, conjunctions, counts), _) =>
      val (ff, conj, cnt) = loop(q, flipFlops, conjunctions, counts)
      (ff, conj, cnt.updated(Low, cnt(Low) + 1))
    }
    ._3
}

final case class RxSearchState(
    flipFlops: Map[String, Boolean],
    conjs: Map[String, Map[String, Pulse]],
    presses: Long,
    cycleLengths: Map[String, Long],
    seen: Map[String, Boolean]
) {
  def minPresses: Option[Long] = Option.when(seen.forall(_._2))(cycleLengths.values.foldLeft(1L)(lcm(_, _)))
}

def findNumberOfButtonPressesForRx(
    graph: Map[String, Vector[(String, ModuleType)]],
    flipFlops: Vector[String],
    conjunctions: Map[String, Set[String]],
    rxOrigin: String
) = {
  def loop(q: Queue[QueueState], state: RxSearchState): RxSearchState = {
    val RxSearchState(flipFlopsState, conjunctionsState, presses, cycleLengths, seen) = state
    if (q.isEmpty) state
    else {
      val (QueueState(origin, target, kind, pulse), q0) = q.dequeue
      val newTargets                                    = graph.get(target).getOrElse(Vector.empty)
      val (newSeen, newCycleLengths) = if (target == rxOrigin && pulse == High) {
        val updatedSeen = seen.updated(origin, true)
        val updatedCycleLengths =
          if (!cycleLengths.contains(origin)) cycleLengths.updated(origin, presses) else cycleLengths
        (updatedSeen, updatedCycleLengths)
      } else {
        (seen, cycleLengths)
      }
      kind match {
        case ModuleType.Broadcaster => ???
        case ModuleType.FlipFlop =>
          pulse match {
            case Low =>
              val newFlipFlopsState = flipFlopsState.updatedWith(target)(_.map(!_))
              val newPulse          = if (newFlipFlopsState(target)) High else Low
              val q1 = q0.enqueueAll(newTargets.map { case (t, k) => QueueState(target, t, k, newPulse) })
              loop(q1, RxSearchState(newFlipFlopsState, conjunctionsState, presses, newCycleLengths, newSeen))
            case High =>
              loop(q0, RxSearchState(flipFlopsState, conjunctionsState, presses, newCycleLengths, newSeen))
          }
        case ModuleType.Conjunction =>
          val newConjunctionsState = conjunctionsState.updatedWith(target)(_.map(_.updated(origin, pulse)))
          val newPulse             = if (newConjunctionsState(target).values.forall(_ == High)) Low else High
          val q1                   = q0.enqueueAll(newTargets.map { case (t, k) => QueueState(target, t, k, newPulse) })
          loop(q1, RxSearchState(flipFlopsState, newConjunctionsState, presses, newCycleLengths, newSeen))
        case ModuleType.DeadEnd =>
          loop(q0, RxSearchState(flipFlopsState, conjunctionsState, presses, newCycleLengths, newSeen))
      }
    }
  }

  val q = Queue(graph("broadcaster").map { case (target, kind) => QueueState("broadcaster", target, kind, Low) }*)
  val flipFlopsState    = flipFlops.map(_ -> false).toMap
  val conjunctionsState = conjunctions.map { case (name, inputs) => name -> (inputs.map(_ -> Low)).toMap }
  val presses           = 0L
  val cycleLengths      = Map.empty[String, Long].withDefaultValue(0L)
  val seen              = conjunctions(rxOrigin).map(_ -> false).toMap
  val initialState      = RxSearchState(flipFlopsState, conjunctionsState, presses, cycleLengths, seen)

  1.to(4000).foldLeft(initialState) { case (state, _) =>
    loop(q, state.copy(presses = state.presses + 1))
  }

  Iterator
    .iterate(initialState)(state => loop(q, state.copy(presses = state.presses + 1)))
    .dropWhile(_.minPresses.isEmpty)
    .take(1)
    .toVector
    .flatMap(_.minPresses)
    .head
}

@main def part1: Unit = {
  val (graph, flipFlops, conjunctions, _) = getInput()
  val counts                              = pushButton(graph, flipFlops, conjunctions, 1000)
  val result                              = counts(Low) * counts(High)
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val (graph, flipFlops, conjunctions, rxOrigin) = getInput()
  val result = findNumberOfButtonPressesForRx(graph, flipFlops, conjunctions, rxOrigin)
  println(s"Part 2 Solution: $result")
}
