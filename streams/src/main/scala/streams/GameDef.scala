package streams

/** This trait represents the layout and building blocks of the game
  */
trait GameDef:

  /** The case class `Pos` encodes positions in the terrain.
    *
    * IMPORTANT NOTE
    *   - The `row` coordinate denotes the position on the vertical axis
    *   - The `col` coordinate is used for the horizontal axis
    *   - The coordinates increase when moving down and right
    *
    * Illustration:
    *
    * ```
    *     0 1 2 3   <- col axis
    *   0 o o o o
    *   1 o o o o
    *   2 o # o o    # is at position Pos(2, 1)
    *   3 o o o o
    *
    *   ^
    *   |
    *   row axis
    * ```
    */
  case class Pos(row: Int, col: Int):
    /** The position obtained by changing the `row` coordinate by `d` */
    def deltaRow(d: Int): Pos = copy(row = row + d)

    /** The position obtained by changing the `col` coordinate by `d` */
    def deltaCol(d: Int): Pos = copy(col = col + d)
  /** We can move left, right, Up or down. These moves are encoded as case
    * objects.
    */
  enum Move:
    case Left, Right, Up, Down

  trait State:
    /** The state obtained by moving left */
    def left: State

    /** The state obtained by moving right */
    def right: State

    /** The state obtained by moving up */
    def up: State

    /** The state obtained by moving down */
    def down: State

    /** Returns the list of states that can be obtained by moving the current
      * block, together with the corresponding move.
      */
    def neighbors: List[(State, Move)]

    /** Returns `true` if the state is entirely inside the terrain.
      */
    def isLegal: Boolean

    /** Returns the list of positions reachable from the current state which are
      * inside the terrain.
      */
    def legalNeighbors: List[(State, Move)] = neighbors

    /** Returns `true` this state is at the final position
      */
    def done: Boolean
  /** The terrain is represented as a function from positions to booleans. The
    * function returns `true` for every position that is inside the terrain.
    *
    * As explained in the documentation of class `Pos`, the `row` axis is the
    * vertical one and increases from top to bottom.
    */
  type Terrain = Pos => Boolean

  /** The terrain of this game. This value is left abstract.
    */
  def terrain: Terrain

  /** This function returns the state at the start position of the game.
    */
  def startState: State
