package streams

class BloxorzSuite extends munit.FunSuite:

  class InfiniteLevelSuit extends Solver with InfiniteTerrain with BloxorzDef:
    val startPos = Pos(1, 3)
    val goal = Pos(5, 8)

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain:
    /** This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves is a
      * valid solution, i.e. leads to the goal.
      */
    import Move.*
    def solve(ls: List[Move]): State =
      ls.foldLeft(startState) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match
          case Left  => block.left
          case Right => block.right
          case Up    => block.up
          case Down  => block.down
      }

  trait Level0 extends BloxorzDef with SolutionChecker:
    /* terrain for level 0*/
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

    import Move.*
    val optsolution = List(Down, Right, Up)

  trait Level1 extends BloxorzDef with SolutionChecker:
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    import Move.*
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)

  test("terrain function level 1 (10pts)"):
    new Level1:
      assertEquals(terrain(Pos(0, 0)), true)
      assertEquals(terrain(Pos(1, 1)), true) // start
      assertEquals(terrain(Pos(4, 7)), true) // goal
      assertEquals(terrain(Pos(5, 8)), true)
      assertEquals(terrain(Pos(5, 9)), false)
      assertEquals(terrain(Pos(4, 9)), true)
      assertEquals(terrain(Pos(6, 8)), false)
      assertEquals(terrain(Pos(4, 11)), false)
      assertEquals(terrain(Pos(-1, 0)), false)
      assertEquals(terrain(Pos(0, -1)), false)

  test("find char level 1 (10pts)"):
    new Level1:
      assertEquals(startPos, Pos(1, 1))
      assertEquals(goal, Pos(4, 7))

  test("isLegal level 1 (5pts)"):
    new Level1:
      assertEquals(startState.isLegal, true)
      assertEquals(BloxorzState(goal, goal).isLegal, true)
      assertEquals(BloxorzState(Pos(0, 3), Pos(0, 4)).isLegal, false)
      assertEquals(BloxorzState(Pos(1, 3), Pos(1, 4)).isLegal, true)

  test("done simple level (5pts)"):
    new InfiniteLevelSuit:
      assertEquals(BloxorzState(Pos(0, 0), Pos(0, 0)).done, false)
      assertEquals(BloxorzState(goal, goal).done, true)
      assertEquals(BloxorzState(startPos, goal).done, false)

  test("neighbors level 1 (5pts)"):
    new Level1:
      import Move.*
      assertEquals(
        startState.neighbors.toSet,
        Set(
          (BloxorzState(Pos(1, -1), Pos(1, 0)), Left),
          (BloxorzState(Pos(1, 2), Pos(1, 3)), Right),
          (BloxorzState(Pos(-1, 1), Pos(0, 1)), Up),
          (BloxorzState(Pos(2, 1), Pos(3, 1)), Down)
        )
      )

  test("legal neighbors level 1 (5pts)"):
    new Level1:
      import Move.*
      assertEquals(
        startState.legalNeighbors.toSet,
        Set(
          (BloxorzState(Pos(1, 2), Pos(1, 3)), Right),
          (BloxorzState(Pos(2, 1), Pos(3, 1)), Down)
        )
      )

  test("neighborsWithHistory level 1 (10pts)"):
    new Level1:
      import Move.*
      assertEquals(
        neighborsWithHistory(BloxorzState(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet,
        Set(
          (BloxorzState(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (BloxorzState(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        )
      )

  test("newNeighborsOnly level 1 (10pts)"):
    new Level1:
      import Move.*
      assertEquals(
        newNeighborsOnly(
          Set(
            (BloxorzState(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
            (BloxorzState(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
          ).to(LazyList),
          Set(BloxorzState(Pos(1, 2), Pos(1, 3)), BloxorzState(Pos(1, 1), Pos(1, 1)))
        ).toSet,
        Set(
          (BloxorzState(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        )
      )

  test("optimal solution for level 0 (5pts)"):
    new Level0:
      assertEquals(solve(solution), BloxorzState(goal, goal))

  test("optimal solution length for level 0 (5pts)"):
    new Level0:
      assertEquals(solution.length, optsolution.length)

  test("optimal solution for level 1 (10pts)"):
    new Level1:
      assertEquals(solve(solution), BloxorzState(goal, goal))

  test("optimal solution length for level 1 (10pts)"):
    new Level1:
      assertEquals(solution.length, optsolution.length)

  test("After completing the lab, please report how long you spent on it"):
    assert(howManyHoursISpentOnThisLab > 0.0)

  import scala.concurrent.duration.*
  override val munitTimeout = 20.seconds
