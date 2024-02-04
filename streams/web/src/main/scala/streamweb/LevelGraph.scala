package streamweb

import streams.GameDef
import streams.StringParserTerrain
import streams.BloxorzDef

import scalajs.js
import scala.scalajs.js
import js.JSConverters.*

class LevelGraph(val level: BloxorzDef & StringParserTerrain):
  case class Graph(nodes: Set[level.State], transitions: List[(level.State, level.State, level.Move)])

  val graph = getGraph

  def getGraph: Graph =
    def explore(from: level.State, acc: Graph): Graph =
      from.legalNeighbors.foldLeft(acc)((graph, b) =>
        if graph.nodes.contains(b._1) then Graph(graph.nodes, (from, b._1, b._2) :: graph.transitions)
        else explore(b._1, Graph(graph.nodes + b._1, (from, b._1, b._2) :: graph.transitions))
      )

    explore(level.startState, Graph(Set.empty, Nil))

  def getTerrain: Seq[(Int, Int)] =
    level.vector.zipWithIndex
      .flatMap((v, i) => v.zipWithIndex.collect { case (v, j) if Seq('o', 'S', 'T').contains(v) => (i, j) })
