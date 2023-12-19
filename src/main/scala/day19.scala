package day19

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.io.Source

final case class Rule(category: String, op: String, amount: Long, next: String) {
  def run(part: Map[String, Long]): Option[String] = op match {
    case "<" => Option.when(part(category) < amount)(next)
    case ">" => Option.when(part(category) > amount)(next)
  }
}

final case class Workflow(name: String, rules: Vector[Rule], fallback: String)

def getInput(): (Map[String, Workflow], Vector[Map[String, Long]]) = {
  val CondPat = """(\w+)(>|<)(\d+)""".r
  def parseWorkflow(s: String): Workflow = {
    s match {
      case s"$name{$condsStr}" =>
        val condParts = condsStr.split(",").toVector
        val rules = condParts.init.map(_.split(":").toVector match {
          case Vector(CondPat(category, op, numStr), next) => Rule(category, op, numStr.toLong, next)
          case _                                           => ???
        })
        Workflow(name, rules, condParts.last)
    }
  }

  val Array(workflowsStr, partsStr) = Source.fromResource("19-input.txt").getLines().mkString("\n").split("\n\n")

  val workflows = workflowsStr.split("\n").map(parseWorkflow).map(wf => wf.name -> wf).toMap
  val parts = partsStr.split("\n").toVector.map { case s"{x=$x,m=$m,a=$a,s=$s}" =>
    Map("x" -> x.toLong, "m" -> m.toLong, "a" -> a.toLong, "s" -> s.toLong)
  }

  (workflows, parts)
}

def runWorkflows(workflows: Map[String, Workflow], parts: Vector[Map[String, Long]]) = {
  @tailrec
  def loop(part: Map[String, Long], curWorkflow: String = "in", result: Long = 0L): Long = {
    curWorkflow match {
      case "A" => part.values.sum
      case "R" => 0L
      case _ =>
        val wf = workflows(curWorkflow)
        val maybeNext = wf.rules.foldLeft(Option.empty[String]) { case (acc, rule) =>
          acc.orElse(rule.run(part))
        }
        maybeNext match {
          case Some(next) => loop(part, next, result)
          case None       => loop(part, wf.fallback, result)
        }
    }
  }

  parts.map(loop(_)).sum
}

def getCombinations(workflows: Map[String, Workflow]): Long = {
  def recur(ranges: Map[String, NumericRange[Long]], next: String = "in"): Long = {
    next match {
      case "A" => ranges.values.map(_.size.toLong).product
      case "R" => 0L
      case _ =>
        val wf = workflows(next)
        val (total, newRanges, _) = wf.rules.foldLeft((0L, ranges, false)) {
          case (state @ (acc, ranges, isDone), Rule(category, op, amount, next)) =>
            if (isDone) state
            else {
              val curRange = ranges(category)
              val (rangeIfTrue, rangeIfFalse) = op match {
                case "<" => (curRange.min until amount, amount to curRange.max)
                case ">" => (amount + 1 to curRange.max, curRange.min to amount)
              }
              val newAcc = acc + (if (rangeIfTrue.nonEmpty) recur(ranges.updated(category, rangeIfTrue), next) else 0L)

              if (rangeIfFalse.nonEmpty) (newAcc, ranges.updated(category, rangeIfFalse), false)
              else (newAcc, ranges, true)
            }
        }
        total + recur(newRanges, wf.fallback)
    }
  }

  recur("xmas".map(_.toString -> (1L to 4000L)).toMap)
}

@main def part1: Unit = {
  val (workflows, parts) = getInput()
  val result             = runWorkflows(workflows, parts)
  println(s"Part 1 Solution: $result")
}

@main def part2: Unit = {
  val (workflows, _) = getInput()
  val result         = getCombinations(workflows)
  println(s"Part 2 Solution: $result")
}
