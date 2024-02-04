package streams

/** A main object that can be used to execute the Bloxorz solver
  */
object Bloxorz:

  /** A level constructed using the `InfiniteTerrain` trait which defines the
    * terrain to be valid at every position.
    */
  object InfiniteLevel extends Solver with InfiniteTerrain with BloxorzDef:
    val startPos = Pos(1, 3)
    val goal = Pos(5, 8)

  /** A simple level constructed using the StringParserTerrain
    */
  abstract class Level extends Solver with StringParserTerrain with BloxorzDef

  object Level0 extends Level:
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

  /** Level 1 of the official Bloxorz game
    */
  object Level1 extends Level:
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

  object Level2 extends Level:
    val level =
      """---o----
        |---oo---
        |oooo-ooo
        |oToooo-S
        |ooo-oooo
        |ooo--ooo""".stripMargin
