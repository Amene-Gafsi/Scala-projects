package streams

/** This component implements the solver for the Bloxorz game
  */
trait Solver extends GameDef:
  /** This function takes two arguments: the current state `s` and a list of
    * moves `history` that was required to reach the position of `s`.
    *
    * The `head` element of the `history` list is the latest move that was
    * executed, i.e. the last move that was performed for the block to end up at
    * state `s`.
    *
    * The function returns a lazy list of pairs: the first element of the each
    * pair is a neighboring block, and the second element is the augmented
    * history of moves required to reach this block.
    *
    * It should only return valid neighbors, i.e. block positions that are
    * inside the terrain.
    */
  def neighborsWithHistory(s: State, history: List[Move]): LazyList[(State, List[Move])] =
    s.legalNeighbors.to(LazyList).map { case (neighbor, move) => (neighbor, move :: history) }

  /** This function returns the list of neighbors without the block positions
    * that have already been explored. We will use it to make sure that we don't
    * explore circular paths.
    */
  def newNeighborsOnly(neighbors: LazyList[(State, List[Move])], explored: Set[State]): LazyList[(State, List[Move])] =
    neighbors.filter { case (neighbor, _) => !explored.contains(neighbor) }
  /** The function `from` returns the lazy list of all possible paths that can
    * be followed, starting at the `head` of the `initial` lazy list.
    *
    * The blocks in the lazy list `initial` are sorted by ascending path length:
    * the block positions with the shortest paths (length of move list) are at
    * the head of the lazy list.
    *
    * The parameter `explored` is a set of block positions that have been
    * visited before, on the path to any of the blocks in the lazy list
    * `initial`. When search reaches a block that has already been explored
    * before, that position should not be included a second time to avoid
    * cycles.
    *
    * The resulting lazy list should be sorted by ascending path length,
    * i.e. the block positions that can be reached with the fewest amount of
    * moves should appear first in the lazy list.
    *
    * Note: the solution should not look at or compare the lengths of different
    * paths - the implementation should naturally construct the correctly sorted
    * lazy list.
    */
  def from(initial: LazyList[(State, List[Move])], explored: Set[State]): LazyList[(State, List[Move])] =
    if (initial.isEmpty) LazyList.empty
      else 
        val next = for {
          (block, moves) <- initial
          neighbor <- newNeighborsOnly(neighborsWithHistory(block, moves), explored)
        } yield neighbor
        initial ++ from(next, explored ++ next.map(_._1))

  /** The lazy list of all paths that begin at the starting block.
    */
  lazy val pathsFromStart: LazyList[(State, List[Move])] =
    from(LazyList((startState, List())), Set(startState))

  /** Returns a lazy list of all possible pairs of the goal block along with the
    * history how it was reached.
    */
  lazy val pathsToGoal: LazyList[(State, List[Move])] =
    from(LazyList((startState, List())), Set(startState)).filter { case (state, _) => state.done }
    
  /** The (or one of the) shortest sequence(s) of moves to reach the goal. If
    * the goal cannot be reached, the empty list is returned.
    *
    * Note: the `head` element of the returned list should represent the first
    * move that the player should perform from the starting position.
    */
  lazy val solution: List[Move] =
    pathsToGoal.headOption.map { case (_, moves) => moves.reverse }.getOrElse(List())
