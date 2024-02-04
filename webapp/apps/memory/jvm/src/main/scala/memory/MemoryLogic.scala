package memory

import scala.util.{Try, Random}

import ujson.Value

import cs214.webapp.*
import cs214.webapp.messages.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer

import memory.*
import scala.compiletime.ops.any
import cats.instances.seq

// Feel free to tweak this value!
private val SHOW_CARDS_PAUSE_MS = 2500

object MemoryStateMachine extends cs214.webapp.StateMachine[MemoryEvent, MemoryState, MemoryView]:

  val name: String = "memory"
  val wire = MemoryWire

  def Deck(cards: String): Vector[String] =
    cards.strip.split(" +").to(Vector)

  val DECKS: Map[String, Vector[String]] = Map(
    "Simple" -> Deck("""
      💫 ⭐️
    """),
    "Stars" -> Deck("""
      💫 ⭐️ 🌟 ✨ ☀️
    """),
    "Animals" -> Deck("""
      🐵 🐒 🦍 🦧 🐶 🐕 🦮 🐕‍🦺
      🐩 🐺 🦊 🦝 🐱 🐈 🐈‍⬛ 🦁
      🐯 🐅 🐆 🐴 🫎 🫏 🐎 🦄
      🦓 🦌 🦬 🐮 🐂 🐃 🐄 🐷
      🐖 🐗 🐽 🐏 🐑 🐐 🐪 🐫
      🦙 🦒 🐘 🦣 🦏 🦛 🐭 🐁
      🐀 🐹 🐰 🐇 🐿️ 🦫 🦔 🦇
      🐻 🐻‍❄️ 🐨 🐼 🦥 🦦 🦨 🦘
      🦡
    """),
    "Birds" -> Deck("""
      🦃 🐔 🐓 🐣 🐤 🐥 🐦 🐧
      🕊️ 🦅 🦆 🦢 🦉 🦤 🪶 🦩
      🦚 🦜 🪽 🐦‍⬛ 🪿
    """),
    "Marine & Reptiles" -> Deck("""
      🐸 🐊 🐢 🦎 🐍 🐲 🐉 🦕
      🦖 🐳 🐋 🐬 🦭 🐟 🐠 🐡
      🦈 🐙 🐚 🪸 🪼 🦀 🦞 🦐
      🦑 🦪
    """),
    "Bugs" -> Deck("""
      🐌 🦋 🐛 🐜 🐝 🪲 🐞 🦗
      🪳 🕷️ 🕸️ 🦂 🦟 🪰 🪱 🦠
    """),
    "Plants" -> Deck("""
      💐 🌸 💮 🪷 🏵️ 🌹 🥀 🌺
      🌻 🌼 🌷 🪻 🌱 🪴 🌲 🌳
      🌴 🌵 🌾 🌿 ☘️ 🍀 🍁 🍂
      🍃 🍄 🪨 🪵
    """)
  )

  // Use any strings you want here — the tests don't check for these specific emoji
  val CARDS: Vector[String] = DECKS("Birds")

  /** Creates a new application state. */
  override def init(clients: Seq[UserId]): MemoryState =
    val shuffledCards = Random.shuffle(CARDS ++ CARDS)
    val initialMemory = MemoryView(
      stateView = StateView.Playing(PhaseView.SelectingCards, clients.head, shuffledCards.map(e => CardView.FaceDown)),
      alreadyMatched = clients.map(userId => userId -> Seq.empty[Card]).toMap
    )

    MemoryState(
      memory = initialMemory,
      winner = None,
      clients = clients,
      cartes = shuffledCards
    )

  override def transition(state: MemoryState)(userId: UserId, event: MemoryEvent): Try[Seq[Action[MemoryState]]] =
    
    Try {
      state match {
        case MemoryState(memory, winner, clients, cartes) if (memory.stateView.asInstanceOf[StateView.Playing].board.forall {
            case CardView.AlreadyMatched(_) => true
            case _ => false
          })  =>
          throw IllegalMoveException("Game is already over")

        case MemoryState(memory, winner, clients, cartes) if memory.stateView == StateView.Playing && memory.stateView.asInstanceOf[StateView.Playing].currentPlayer != userId =>
          throw NotYourTurnException()

        case MemoryState(memory, winner, clients, cartes) if event.isInstanceOf[MemoryEvent.Toggle] =>
          val toggle = event.asInstanceOf[MemoryEvent.Toggle]
          val cardId = toggle.cardId
          val board = memory.stateView.asInstanceOf[StateView.Playing].board
      //    || board(cardId) == CardView.FaceDown
          if (cardId < 0 || cardId >= board.length || board.count(_ == CardView.Selected) == 2){
            throw IllegalMoveException("Invalid card selection")
          } else {
              // Toggle face-down card to selected
              val updatedBoard = {
                if (board(cardId) == CardView.Selected) {
                  board.updated(cardId, CardView.FaceDown) 
                } else {
                  board.updated(cardId, CardView.Selected)
                }
              }
              val newMemory = 
              {
                if ( updatedBoard.count(_ == CardView.Selected) == 2 ) { 
                  MemoryView(StateView.Playing(PhaseView.CardsSelected, userId, updatedBoard), memory.alreadyMatched)
                } else {
                  MemoryView(StateView.Playing(PhaseView.SelectingCards, userId, updatedBoard), memory.alreadyMatched)
                }
              }
              Seq(Action.Render(MemoryState(newMemory, winner, clients, cartes)))
          }
          
        case MemoryState(memory, winner, clients, cartes) if event == MemoryEvent.FlipSelected =>
            val board = memory.stateView.asInstanceOf[StateView.Playing].board
            var newBoard = faceUpCards(board, cartes)

            val (newBoard2, newMatch2) = checkMatch(newBoard, cartes)
            val oldScore = memory.alreadyMatched(userId)
            val newScore = oldScore ++ newMatch2
            val newEvents = memory.alreadyMatched.updated(userId, newScore.toSeq)
            val newTurn = if (oldScore == newScore) playerTurn(userId, clients) else userId
            val play = if (oldScore == newScore) PhaseView.BadMatch else PhaseView.GoodMatch
            val newMemory1 = MemoryView(StateView.Playing(play, userId, newBoard), memory.alreadyMatched)

            val newMemory2 = MemoryView(StateView.Playing(play, newTurn, newBoard2), newEvents)
            val win = {
              if (newBoard2.forall{
                case CardView.AlreadyMatched(_) => true
                case _ => false
              }){
                Some("check")
              } else None
            }
            Seq(Action.Render(MemoryState(newMemory1, winner, clients, cartes)), Action.Pause(10000), Action.Render(MemoryState(newMemory2, win, clients, cartes)))
 
        case _ => throw IllegalMoveException("Game is already over")

          
    }}

  override def project(state: MemoryState)(userId: UserId): MemoryView =
    state match {
      case MemoryState(memory, winner, clients, cartes) =>
        if (memory.stateView.isInstanceOf[StateView.Playing]){
          val board = memory.stateView.asInstanceOf[StateView.Playing].board
          if (board.forall {
            case CardView.AlreadyMatched(_) => true
            case _ => false
          }) { 
          val alreadyMatched = memory.alreadyMatched
          val maxMatchedCards = alreadyMatched.values.map(_.length).maxOption.getOrElse(0)
          val winnerIds = alreadyMatched.collect {
          case (userId, cards) if cards.length == maxMatchedCards => userId
          }.toSet
          MemoryView(StateView.Finished(winnerIds), memory.alreadyMatched)
          } else memory
        } else memory         
      
      }


  private def playerTurn(userId: UserId, clients: Seq[UserId]): UserId = {
    val index = clients.indexOf(userId)
    val nextIndex = (index + 1) % clients.length
    clients(nextIndex)
  }

  private def faceUpCards(board: Seq[CardView], cards: Seq[Card]): Seq[CardView] = {
  val selectedIndices = board.zipWithIndex.collect { case (CardView.Selected, idx) => idx }
  board.zipWithIndex.map {
    case (CardView.Selected, idx) => CardView.FaceUp(cards(idx))
    case (other, _) => other
    }
  } 

  private def checkMatch(board: Seq[CardView], cards: Seq[Card]): (Seq[CardView], Seq[Card]) = {
 
    val matchedCards = board.collect {
    case CardView.FaceUp(x) => x
    }

    if matchedCards(0) == matchedCards(1) then 
      (board.map(e => {
      e match
        case CardView.FaceUp(x) => CardView.AlreadyMatched(x)
        case others =>  others
      }), matchedCards)
    else (board.map(e => {
      e match
        case CardView.FaceUp(x) => CardView.FaceDown
        case others =>  others
      }), List.empty[Card].toSeq)
  }



// Server registration magic
class register:
  WebServer.register(MemoryStateMachine)
