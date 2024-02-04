package streams

/** Bloxorz game definition
  */
trait BloxorzDef extends GameDef:

  /** The target position where the state has to go. This value is left
    * abstract.
    */
  def goal: Pos

  /** The position where the state is located initially.
    *
    * This value is left abstract, it will be defined in concrete instances of
    * the game.
    */
  def startPos: Pos
  /** @inheritdoc */
  override def startState: BloxorzState = BloxorzState(startPos, startPos) 

  /** In Bloxorz, a state is represented by the position of the two cubes that
    * it consists of. We make sure that `b1` is lexicographically smaller than
    * `b2`.
    */
  case class BloxorzState(b1: Pos, b2: Pos) extends State:

    // checks the requirement mentioned above
    require(b1.row <= b2.row && b1.col <= b2.col, "Invalid block position: b1=" + b1 + ", b2=" + b2)

    /** Returns a block where the `row` coordinates of `b1` and `b2` are changed
      * by `d1` and `d2`, respectively.
      */
    private def deltaRow(d1: Int, d2: Int) = BloxorzState(b1.deltaRow(d1), b2.deltaRow(d2))

    /** Returns a block where the `col` coordinates of `b1` and `b2` are changed
      * by `d1` and `d2`, respectively.
      */
    private def deltaCol(d1: Int, d2: Int) = BloxorzState(b1.deltaCol(d1), b2.deltaCol(d2))
    /** @inheritdoc */
    override def left =
      if isStanding then deltaCol(-2, -1)
      else if b1.row == b2.row then deltaCol(-1, -2)
      else deltaCol(-1, -1)

    /** @inheritdoc */
    override def right =
      if isStanding then deltaCol(1, 2)
      else if b1.row == b2.row then deltaCol(2, 1)
      else deltaCol(1, 1)

    /** @inheritdoc */
    override def up =
      if isStanding then deltaRow(-2, -1)
      else if b1.row == b2.row then deltaRow(-1, -1)
      else deltaRow(-1, -2)

    /** @inheritdoc */
    override def down =
      if isStanding then deltaRow(1, 2)
      else if b1.row == b2.row then deltaRow(1, 1)
      else deltaRow(2, 1)
    /** Returns `true` if the block is standing.
      */
    def isStanding: Boolean = b1 == b2
    /** @inheritdoc */
    override def done: Boolean =
      b1 == goal && b2 == goal

    /** @inheritdoc */
    override def neighbors: List[(State, Move)] =
      val possibleMoves = List(Move.Left, Move.Right, Move.Up, Move.Down)
      possibleMoves.flatMap { move =>
        val nextState = move match {
          case Move.Left => left
          case Move.Right => right
          case Move.Up => up
          case Move.Down => down
        } 
        Some((nextState, move)) 
      }

    /** @inheritdoc */
    override def legalNeighbors: List[(State, Move)] =
      neighbors.filter { case (nextState, _) => nextState.isLegal }

    /** @inheritdoc */
    override def isLegal: Boolean = terrain(b1) && terrain(b2)
