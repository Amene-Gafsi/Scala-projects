package tictactoe

import ujson.*
import scala.util.{Failure, Success, Try}

import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException
import cs214.webapp.{AppWire, WireFormat, UserId}

object TicTacToeWire extends AppWire[TicTacToeEvent, TicTacToeView]:
  import TicTacToeEvent.*
  import TicTacToeView.*

  override object eventFormat extends WireFormat[TicTacToeEvent]:
    override def encode(t: TicTacToeEvent): Value =
      t match
        case TicTacToeEvent.Move(x, y) => ujson.Arr(ujson.Str("Move"), ujson.Num(x), ujson.Num(y))

    override def decode(json: Value): Try[TicTacToeEvent] =
      Try {
      val x = json(1).num.toInt
      val y = json(2).num.toInt
      TicTacToeEvent.Move(x, y)}
        
  override object viewFormat extends WireFormat[TicTacToeView]:

    def encode(t: TicTacToeView): Value =
      t match
        case TicTacToeView.Playing(board, yourTurn) => ujson.Arr(ujson.Str("Playing"), encodeBoard(board), ujson.Bool(yourTurn))
        case TicTacToeView.Finished(winner) if winner != None => ujson.Arr(ujson.Str("Finished"), ujson.Str(winner.get.toString))
        case TicTacToeView.Finished(winner) => ujson.Arr(ujson.Null)


    private def encodeBoard(board: Board): Value = ujson.Arr(board.cells.map(row => ujson.Arr(row.map(encodeCell): _*)): _*)
    private def encodeCell(cell: Option[UserId]): Value = cell.map(userId => ujson.Str(userId.toString)).getOrElse(ujson.Null)
  

    def decode(json: Value): Try[TicTacToeView] =
      Try {
      if json == ujson.Arr(ujson.Null) then TicTacToeView.Finished(None)
      else
        json(0).str match 
          case "Playing" =>
            val board = decodeBoard(json(1))
            val yourTurn = json(2).bool
            TicTacToeView.Playing(board, yourTurn)

          case "Finished" =>
            val winner = Some(json(1).str)
            TicTacToeView.Finished(winner)
      }

    private def decodeBoard(json: Value): Board = Board(json.arr.map(row => row.arr.map(decodeCell).toVector).toVector)
    private def decodeCell(json: Value): Option[UserId] = if (json.isNull) None else Some(json.str)
