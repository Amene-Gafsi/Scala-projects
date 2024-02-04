package memory

import scala.util.{Failure, Success, Try}

import cs214.webapp.*
import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException

object MemoryWire extends AppWire[MemoryEvent, MemoryView]:
  import MemoryEvent.*
  import MemoryView.*
  import ujson.*

  override object eventFormat extends WireFormat[MemoryEvent]:
    override def encode(event: MemoryEvent): Value =
      event match
        case MemoryEvent.Toggle(cardId) => ujson.Arr(ujson.Str("toggle"), ujson.Num(cardId))
        case MemoryEvent.FlipSelected => ujson.True

      
    override def decode(js: Value): Try[MemoryEvent] =
      Try {
      if js == ujson.True then MemoryEvent.FlipSelected
      else
        val x = js(1).num.toInt
        MemoryEvent.Toggle(x)      
      }

  override object viewFormat extends WireFormat[MemoryView]:

    override def encode(v: MemoryView): Value =   
      val state = v.stateView
      val already = v.alreadyMatched
      val stateEncoded = state match
        case StateView.Playing(phase, currentPlayer, board) =>
          val playing = ujson.Str("Playing")
          val phaseEncoded = encodePhase(phase)
          val playerEncoded = ujson.Str(currentPlayer.toString())
          val boardEncoded = ujson.Arr(board.map(e => encodeCardView(e)))
          ujson.Obj(
            "state" -> playing,
            "phase" -> phaseEncoded,
            "currentPlayer" -> playerEncoded,
            "board" -> boardEncoded
          )
        case StateView.Finished(winnerIds) => ujson.Obj("Finished" -> ujson.Arr(winnerIds.map(e => ujson.Str(e))))

      val alreadyEncoded = already.map { 
        (userId, cards) => 
          val userEncoded = ujson.Str(userId.toString)
          val cardsEncoded = ujson.Arr(cards.map(e => ujson.Str(e)))
          ujson.Obj("userId" -> userEncoded, "cards" -> cardsEncoded)
      }
      ujson.Arr(stateEncoded, alreadyEncoded)
      
      
    def encodePhase(phase: PhaseView): Value =
      phase match {
        case PhaseView.SelectingCards => ujson.Str("SelectingCards")
        case PhaseView.CardsSelected => ujson.Str("CardsSelected")
        case PhaseView.Waiting => ujson.Str("Waiting")
        case PhaseView.GoodMatch => ujson.Str("GoodMatch")
        case PhaseView.BadMatch => ujson.Str("BadMatch")
      }
    def encodeCardView(cardView: CardView): Value =
      cardView match {
        case CardView.FaceDown => ujson.Str("FaceDown")
        case CardView.Selected => ujson.Str("Selected")
        case CardView.FaceUp(card) => ujson.Arr(ujson.Str("FaceUp"), ujson.Str(card))
        case CardView.AlreadyMatched(card) => ujson.Arr(ujson.Str("AlreadyMatched"), ujson.Str(card))
      }

    override def decode(js: Value): Try[MemoryView] = 
      Try{
          val jsArr = js.arr
          val stateEncoded = jsArr(0).obj
          val alreadyEncoded = jsArr(1)

          val state = {
            if stateEncoded.contains("state") then 
              val phaseDecoded = decodePhase(stateEncoded("phase"))
              val playerDecoded = stateEncoded("currentPlayer").str
              val boardDecoded = stateEncoded("board")(0).arr.map(e => decodeCardView(e)).toVector
              StateView.Playing(phaseDecoded, playerDecoded, boardDecoded)
            else 
              val winnersEncoded = stateEncoded("Finished")
              StateView.Finished(winnersEncoded.arr.flatMap(e => e.arr.map(_.str)).toSet)
            }
          val alreadyScores = alreadyEncoded.arr.map { userJson =>
              val userId = userJson("userId").str
              val cards = userJson("cards").arr.headOption.getOrElse(ujson.Arr()).arr.map(_.str).toSeq
              userId -> cards
            }.toMap
          MemoryView(state, alreadyScores)}
    

    def decodeCardView(js: Value): CardView =
      if js == ujson.Str("FaceDown") then CardView.FaceDown
      else if js == ujson.Str("Selected") then CardView.Selected
      else if js.arr(0) == ujson.Str("FaceUp") then CardView.FaceUp(js.arr(1).str)
      else CardView.AlreadyMatched(js.arr(1).str)

    def decodePhase(js: Value): PhaseView =
      js.str match {
        case "SelectingCards" => PhaseView.SelectingCards
        case "CardsSelected" => PhaseView.CardsSelected
        case "Waiting" => PhaseView.Waiting
        case "GoodMatch" => PhaseView.GoodMatch
        case "BadMatch" => PhaseView.BadMatch
      }