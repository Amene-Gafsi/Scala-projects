package streamweb

import streamweb.Objects3D.*
import streams.GameDef
import streams.BloxorzDef
import streams.Bloxorz
import streams.StringParserTerrain

class Level3D(val level: String, renderer: Renderer) extends Bloxorz.Level:
  var block = startState
  val terrainObjects = makeTerrain

  def move(dir: Move): Unit =
    block = block.legalNeighbors
      .find(_._2 == dir)
      .map(_._1)
      .getOrElse(block)
      .asInstanceOf[BloxorzState]

  def reset(): Unit =
    block = startState

  def render(): Unit =
    val objects = terrainObjects ++ makeBlock
    val pov = Vec3D(Math.min(block.b1.row, block.b2.row).toFloat, Math.min(block.b1.col, block.b2.col).toFloat, 0.0)
    renderer.drawScene(objects, pov)

  def makeTerrain =
    terrainBlock(startPos.row, startPos.col).withTexture("wood") +:
      terrainBlock(goal.row, goal.col).withTexture("gold") +:
      vector.zipWithIndex
        .flatMap((v, i) => v.zipWithIndex.collect { case ('o', j) => (i, j) })
        .map((x, y) => terrainBlock(x, y).withTexture("stone"))

  def makeBlock =
    Object3D.Box
      .translate(Vec3D(block.b1.row.toFloat, block.b1.col.toFloat, 0.0)).withTexture("diamond") +:
      Object3D.Box
        .translate(
          Vec3D(
            block.b2.row.toFloat,
            block.b2.col.toFloat,
            if block.b1 == block.b2 then 1.0 else 0.0
          )
        ).withTexture("diamond") +: Seq.empty

  def terrainBlock(x: Int, y: Int) =
    Object3D.Box.scale(Vec3D(1.0, 1.0, 0.2)).translate(Vec3D(x.toFloat, y.toFloat, -0.2))
