package day24

import z3.scala.*

import scala.io.Source

final case class Point3D(x: Long, y: Long, z: Long)

final case class Hailstone(p: Point3D, v: Point3D) {
  val slope: Double     = v.y / v.x.toDouble
  val intercept: Double = p.y - slope * p.x
}

def getInput(): Vector[Hailstone] = {
  Source
    .fromResource("24-input.txt")
    .getLines()
    .map { case s"$px, $py, $pz @ $vx, $vy, $vz" =>
      Hailstone(
        Point3D(px.trim.toLong, py.trim.toLong, pz.trim.toLong),
        Point3D(vx.trim.toLong, vy.trim.toLong, vz.trim.toLong)
      )
    }
    .toVector
}

def countIntersections(hailstones: Vector[Hailstone], testArea: (Double, Double)): Int = {
  hailstones.combinations(2).count { v =>
    val Vector(h1, h2) = v
    val isParallel     = h1.slope == h2.slope
    if (isParallel) false
    else {
      val xInter                     = (h2.intercept - h1.intercept) / (h1.slope - h2.slope)
      val yInter                     = h1.slope * xInter + h1.intercept
      val t1                         = (xInter - h1.p.x) / h1.v.x
      val t2                         = (xInter - h2.p.x) / h2.v.x
      val isBehindStartingPoints     = t1 < 0 || t2 < 0
      val (testAreaMin, testAreaMax) = testArea
      val xInTestArea                = testAreaMin <= xInter && xInter <= testAreaMax
      val yInTestArea                = testAreaMin <= yInter && yInter <= testAreaMax
      !isBehindStartingPoints && xInTestArea && yInTestArea
    }
  }
}

def throwMagicalRock(hailstones: Vector[Hailstone]): String = {
  val z3 = new Z3Context("MODEL" -> true)
  val i  = z3.mkIntSort()
  val px = z3.mkConst("px", i)
  val py = z3.mkConst("py", i)
  val pz = z3.mkConst("pz", i)
  val vx = z3.mkConst("vx", i)
  val vy = z3.mkConst("vy", i)
  val vz = z3.mkConst("vz", i)
  val constraints = hailstones.zipWithIndex.take(3).foldLeft(z3.mkTrue()) { case (acc, (h, k)) =>
    val tk = z3.mkConst(s"t$k", i)
    val xConstraint = z3.mkEq(
      z3.mkAdd(px, z3.mkMul(tk, vx)),
      z3.mkAdd(z3.mkNumeral(h.p.x.toString, i), z3.mkMul(tk, z3.mkNumeral(h.v.x.toString, i)))
    )
    val yConstraint = z3.mkEq(
      z3.mkAdd(py, z3.mkMul(tk, vy)),
      z3.mkAdd(z3.mkNumeral(h.p.y.toString, i), z3.mkMul(tk, z3.mkNumeral(h.v.y.toString, i)))
    )
    val zConstraint = z3.mkEq(
      z3.mkAdd(pz, z3.mkMul(tk, vz)),
      z3.mkAdd(z3.mkNumeral(h.p.z.toString, i), z3.mkMul(tk, z3.mkNumeral(h.v.z.toString, i)))
    )
    z3.mkAnd(acc, z3.mkAnd(xConstraint, yConstraint, zConstraint))
  }
  val solver = z3.mkSolver()
  solver.assertCnstr(constraints)
  solver
    .check()
    .flatMap { isModelAvailable =>
      if (isModelAvailable) solver.getModel().eval(z3.mkAdd(px, py, pz))
      else None
    }
    .map(_.toString)
    .getOrElse("No solution.")
}

@main def part1(): Unit = {
  val result = countIntersections(getInput(), (200000000000000.0, 400000000000000.0))
  println(s"Part 1 Solution: $result")
}

@main def part2(): Unit = {
  val result = throwMagicalRock(getInput())
  println(s"Part 2 Solution: $result")
}
