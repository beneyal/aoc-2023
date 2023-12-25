package day25

import org.jgrapht.alg.StoerWagnerMinimumCut
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.DefaultUndirectedGraph
import scala.io.Source

def getInput() = {
  val graph = DefaultUndirectedGraph[String, DefaultEdge](DefaultEdge().getClass)
  Source
    .fromResource("25-input.txt")
    .getLines()
    .foreach { case s"${src}: $targets" =>
      graph.addVertex(src)
      targets.split(" ").foreach { t =>
        graph.addVertex(t)
        graph.addEdge(src, t)
      }
    }

  graph
}

@main def part1(): Unit = {
  val graph  = getInput()
  val minCut = StoerWagnerMinimumCut(graph).minCut()
  graph.removeAllVertices(minCut)
  val result = graph.vertexSet.size * minCut.size
  println(s"Part 1 Solution: $result")
}

@main def part2(): Unit = {
  val result = "BUTTON!!!"
  println(s"Part 2 Solution: $result")
}
