package othello

import Game._

import scala.concurrent.Future
import scala.concurrent.duration._

import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.{Json, JsValue}
import play.api.libs.json.Json.toJson
import play.api.libs.iteratee.{Concurrent, Enumerator, Iteratee}
import play.api.libs.concurrent.Akka
import akka.actor.{ActorRef, Actor, Props}
import play.api.libs.iteratee.Concurrent.Channel
import scala.util.Random

trait Player

case class HumanPlayer(color: Color, channel: Channel[JsValue], controller: ActorRef) extends Player with Actor {
  def receive = {
    case GameUpdated(game) => {
      channel.push(toJson(game))
    }
    case UserEvent(value) => {
      Json.fromJson[Pos](value).map { pos =>
        controller ! PlayPos(color, pos)
      }
    }
  }
}

object HumanPlayer {
  def init(color: Color, controller: ActorRef): (ActorRef, Iteratee[JsValue,_], Enumerator[JsValue]) = {
    val (enumerator, channel) = Concurrent.broadcast[JsValue]
    val humanPlayer = Akka.system.actorOf(Props{new HumanPlayer(White, channel, controller)});
    val iteratee = Iteratee.foreach[JsValue]{ value =>
      humanPlayer ! UserEvent(value)
    }
    (humanPlayer, iteratee, enumerator)
  }
}

case class BotPlayer(color: Color, controller: ActorRef, delay: FiniteDuration = 1 seconds) extends Player with Actor {
  def receive = {
    case GameUpdated(game) => {
      if (game.nextColor == color) {
        val moves = game.board.possibleMoves(color).toVector
        if (moves.nonEmpty) {
          val (randomPos,_) = moves(Random.nextInt(moves.size))
          context.system.scheduler.scheduleOnce(delay) {
            controller ! PlayPos(color, randomPos)
          }
        }
      }
    }
  }
}

class GameController(game: Game) extends Actor {
  var currentGame: Game = game
  var players = Map[Color, ActorRef]()

  def receive = {
    case RegisterPlayer(color, player) => {
      players = players + (color -> player)
      player ! GameUpdated(currentGame)
    }
    case PlayPos(color, pos) => {
      if (color == currentGame.nextColor && sender == players(color)) {
        currentGame = currentGame.play(pos)
        players.values.foreach {
          _ ! GameUpdated(currentGame)
        }
      }
    }
  }
}

case class GameUpdated(game: Game)
case class RegisterPlayer(color: Color, player: ActorRef)
case class PlayPos(color: Color, pos: Pos)
case class UserEvent(value: JsValue)
