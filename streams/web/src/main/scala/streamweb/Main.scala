package streamweb

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import org.scalajs.dom.window

import scalajs.js
import scala.scalajs.js
import js.JSConverters.*

import org.scalajs.dom.KeyboardEvent
import streams.GameDef
import streams.Bloxorz.*
import streams.BloxorzDef
import streams.StringParserTerrain
import org.scalajs.dom.WebGLRenderingContext
import scala.collection.mutable

object Main:

  val levels: Map[Int, Level] = Map(
    0 -> Level0,
    1 -> Level1,
    2 -> Level2
  )

  // Change this to the level you want to display
  val displayLevel = levels(1)

  def main(args: Array[String]): Unit =
    val canvas = document.querySelector("#glcanvas").asInstanceOf[html.Canvas]
    val gl = canvas.getContext("webgl").asInstanceOf[WebGLRenderingContext]

    val renderer = Renderer(gl)
    val level3D = Level3D(displayLevel.level, renderer)
    window.setTimeout(() => display3D(level3D), 1000)

    displayGraph(displayLevel)

  def display3D(level: Level3D) =
    level.render()

    val sol = List(
      level.Move.Down,
      level.Move.Left,
      level.Move.Left,
      level.Move.Up,
      level.Move.Up,
      level.Move.Right,
      level.Move.Up,
      level.Move.Left,
      level.Move.Up,
      level.Move.Up,
      level.Move.Left,
      level.Move.Up,
      level.Move.Left,
      level.Move.Down,
      level.Move.Right,
      level.Move.Up,
      level.Move.Left,
      level.Move.Left,
      level.Move.Left,
      level.Move.Down,
      level.Move.Down,
      level.Move.Down,
      level.Move.Left,
      level.Move.Down,
      level.Move.Left,
      level.Move.Left,
      level.Move.Down,
      level.Move.Left,
      level.Move.Up
    )

    def move(dir: level.Move) =
      level.move(dir)
      level.render()

    document.addEventListener(
      "keydown",
      e =>
        e.asInstanceOf[KeyboardEvent].key match
          case "ArrowDown"  => move(level.Move.Down)
          case "ArrowUp"    => move(level.Move.Up)
          case "ArrowRight" => move(level.Move.Right)
          case "ArrowLeft"  => move(level.Move.Left)
          case "r" =>
            level.reset()
            level.render()
          case "s" =>
            def progress(seq: List[level.Move]): Unit = seq match
              case head :: next =>
                window.setTimeout(() => progress(next), 1000)
                move(head)
              case Nil =>
            level.reset()
            level.render()
            progress(sol)
          case _ =>

    )

  def displayGraph(level: StringParserTerrain & BloxorzDef) =
    val graph = LevelGraph(level)

    def id(b: level.BloxorzState) =
      (b.b1.col, b.b1.row, b.b2.col, b.b2.row).toString

    val visited = mutable.Set(level.startState)
    val used = mutable.Set.empty[(level.State, level.State, level.Move)]
    var toVisit = mutable.Set(level.startState)

    def jsData() =
      val nodes = js.Array(graph.graph.nodes.map(b =>
        js.Dictionary[Any](
          "posX" -> ((b.asInstanceOf[level.BloxorzState].b1.col + b.asInstanceOf[
            level.BloxorzState
          ].b2.col) / 2.0 + 0.5),
          "posY" -> ((b.asInstanceOf[level.BloxorzState].b1.row + b.asInstanceOf[
            level.BloxorzState
          ].b2.row) / 2.0 + 0.5),
          "x1" -> b.asInstanceOf[level.BloxorzState].b1.col,
          "x2" -> b.asInstanceOf[level.BloxorzState].b2.col,
          "y1" -> b.asInstanceOf[level.BloxorzState].b1.row,
          "y2" -> b.asInstanceOf[level.BloxorzState].b2.row,
          "id" -> id(b.asInstanceOf[level.BloxorzState]),
          "description" -> (
            if b.asInstanceOf[level.BloxorzState].isStanding && b.asInstanceOf[level.BloxorzState].b1 == level.startPos
            then "start"
            else if b.asInstanceOf[level.BloxorzState].isStanding && b.asInstanceOf[level.BloxorzState].b1 == level.goal
            then "goal"
            else if b.asInstanceOf[level.BloxorzState].isStanding then "standing"
            else "laying"
          ),
          "visited" -> visited.contains(b.asInstanceOf[level.BloxorzState])
        )
      ).toSeq*).sortBy(_("id").asInstanceOf[String])

      val links = graph.graph.transitions.map((from, to, move) =>
        js.Dictionary[Any](
          "source" -> id(from.asInstanceOf[level.BloxorzState]),
          "target" -> id(to.asInstanceOf[level.BloxorzState]),
          "used" -> used.contains((
            from.asInstanceOf[level.BloxorzState],
            to.asInstanceOf[level.BloxorzState],
            move.asInstanceOf[level.Move]
          ))
        )
      ).toJSArray

      val background = graph.getTerrain.map((i, j) =>
        js.Dictionary[Any](
          "y" -> i,
          "x" -> j
        )
      ).toJSArray

      (nodes, links, background)

    val (nodes, links, background) = jsData()
    val visualization = GraphVisualization(nodes, links, background)

    document.querySelector("#animate").addEventListener(
      "click",
      e =>
        val toVisitNext = mutable.Set.empty[level.BloxorzState]
        for b <- toVisit do
          for next <- b.legalNeighbors do
            if !visited.contains(next._1.asInstanceOf[level.BloxorzState]) then
              toVisitNext.add(next._1.asInstanceOf[level.BloxorzState])
              visited.add(next._1.asInstanceOf[level.BloxorzState])
              used.add((b, next._1, next._2))

        toVisit = toVisitNext

        val (nodes, links, _) = jsData()

        visualization.animate(nodes, links)
    )

    document.querySelector("#toggle-layout").addEventListener("click", e => visualization.toggleLayout())
