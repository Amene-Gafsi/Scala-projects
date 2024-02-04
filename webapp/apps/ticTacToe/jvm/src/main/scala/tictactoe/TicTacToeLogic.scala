package tictactoe

import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

import cs214.webapp.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer
import cs214.webapp.messages.Action

object TicTacToeStateMachine extends cs214.webapp.StateMachine[TicTacToeEvent, TicTacToeState, TicTacToeView]:

  val name: String = "tictactoe"
  val wire = TicTacToeWire

  override def init(clients: Seq[UserId]): TicTacToeState =
    TicTacToeState(Board(Vector.fill(3, 3)(None)), clients.head, None, clients)


  // Failures in the Try must hold instances of AppException
  // (from Exceptions.scala under lib/shared/)
  override def transition(state: TicTacToeState)(uid: UserId, event: TicTacToeEvent): Try[Seq[Action[TicTacToeState]]] =
    Try {
    state match {
      case TicTacToeState(board, currentPlayer, winner, clients) if board.cells.flatten.forall(_.isDefined) =>
      throw IllegalMoveException("Game is already over")

      case TicTacToeState(board, currentPlayer, winner, clients) if currentPlayer != uid =>
        throw NotYourTurnException()
      
      case TicTacToeState(board, currentPlayer, winner, clients) if event.isInstanceOf[TicTacToeEvent.Move] =>
        val move = event.asInstanceOf[TicTacToeEvent.Move]
        val x = move.x
        val y = move.y

        try {
          val currentCellValue = board.apply(x, y)

          if (currentCellValue.isDefined) throw IllegalMoveException("Cell already filled")
          else {
            val updatedBoard = Board(board.cells.updated(x, board.cells(x).updated(y, Some(uid))))
            val newWinner = findWinner(updatedBoard.cells)
            if newWinner != None then Seq(Action.Render(TicTacToeState(updatedBoard, playerTurn(uid, clients), newWinner, clients)))
            else 
              Seq(Action.Render(TicTacToeState(updatedBoard, playerTurn(uid, clients), None, clients)))
          }
        } catch e => throw IllegalMoveException("Invalid cell coordinates")


      case _ =>
        throw DecodingException("Unexpected event")
    }}

  override def project(state: TicTacToeState)(uid: UserId): TicTacToeView =
    state match {
    case TicTacToeState(board, currentPlayer, winner, clients) if winner != None || board.cells.flatten.forall(_.isDefined) =>
      TicTacToeView.Finished(winner)

    case TicTacToeState(board, currentPlayer, winner, clients) =>
      TicTacToeView.Playing(board, (currentPlayer == uid))
  }

  def playerTurn(uid: UserId, clients: Seq[UserId]): UserId =
    val index = clients.indexOf(uid)
    val nextIndex = (index + 1) % clients.length
    clients(nextIndex)

  def findWinner(cells: Vector[Vector[Option[UserId]]]): Option[UserId] = {
    def checkLine(line: Vector[Option[UserId]]): Option[UserId] =
      if (line.forall(_.isDefined) && line.toSet.size == 1) line.head
      else None

    val allLines = cells ++ 
                    cells.transpose :+ 
                    (0 until cells.length).map(i => cells(i)(i)).toVector :+ 
                    (0 until cells.length).map(i => cells(i)(cells.length - i - 1)).toVector

    allLines.flatMap(checkLine).headOption
  }

  /*def findWinner(cells: Vector[Vector[Option[UserId]]]): Option[UserId] = {
    if (cells(0)(0).isDefined && cells(1)(1).isDefined && cells(2)(2).isDefined && cells(0)(0) == cells(1)(1) && cells(1)(1) == cells(2)(2)) then cells(1)(1)
    else if (cells(0)(2).isDefined && cells(1)(1).isDefined && cells(2)(0).isDefined && cells(2)(0) == cells(1)(1) && cells(1)(1) == cells(0)(2)) then cells(1)(1)
    else if (cells(0)(0).isDefined && cells(0)(1).isDefined && cells(0)(2).isDefined && cells(0)(0) == cells(0)(1) && cells(0)(1) == cells(0)(2)) then cells(0)(1)
    else if (cells(1)(0).isDefined && cells(1)(1).isDefined && cells(1)(2).isDefined && cells(1)(0) == cells(1)(1) && cells(1)(1) == cells(1)(2)) then cells(1)(1)
    else if (cells(2)(0).isDefined && cells(2)(1).isDefined && cells(2)(2).isDefined && cells(2)(0) == cells(2)(1) && cells(2)(1) == cells(2)(2)) then cells(2)(1)
    else if (cells(0)(0).isDefined && cells(1)(0).isDefined && cells(2)(0).isDefined && cells(0)(0) == cells(1)(0) && cells(1)(0) == cells(2)(0)) then cells(1)(0)
    else if (cells(0)(1).isDefined && cells(1)(1).isDefined && cells(2)(1).isDefined && cells(0)(1) == cells(1)(1) && cells(1)(1) == cells(2)(1)) then cells(1)(1)
    else if (cells(0)(2).isDefined && cells(1)(2).isDefined && cells(2)(2).isDefined && cells(0)(2) == cells(1)(2) && cells(1)(2) == cells(2)(2)) then cells(1)(2)
    else None
  }*/

// Server registration magic
class register:
  WebServer.register(TicTacToeStateMachine)
