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
import play.api.Logger

trait PlayerActor extends Actor {
  var color: Option[Color] = None
  var controller: Option[ActorRef] = None

  def baseReceive: Receive = {
    case NotifyController(col,con) => {
      color = Some(col)
      controller = Some(con)
    }
  }
}

case class BotPlayer(delay: FiniteDuration = 1 seconds) extends PlayerActor with Actor {

  def receive = baseReceive.orElse {
    case GameUpdated(game) => {
      color.map { c =>
        if (game.nextColor == c) {
          val moves = game.board.possibleMoves(c).toVector
          if (moves.nonEmpty) {
            val (randomPos,_) = moves(Random.nextInt(moves.size))
            context.system.scheduler.scheduleOnce(delay) {
              controller.map { _ ! PlayPos(c, randomPos) }
            }
          }
        }
      }
    }
  }
}

case class HumanPlayer(channel: Channel[JsValue]) extends PlayerActor with Actor {
  def receive = baseReceive.orElse {
    case GameUpdated(game) => {
      channel.push(toJson(game))
    }
    case UserEvent(value) => {
      color.map { c =>
        Json.fromJson[Pos](value).map { pos =>
          controller.map { _ ! PlayPos(c, pos) }
        }
      }
    }
  }
}

class GameController(game: Game) extends Actor {
  import context._
  var currentGame: Game = game
  var players = Map[Color, ActorRef]()
  var listeners = Set[ActorRef]()

  def receive = {
    case RegisterPlayer(player) if (players.size < 2) => {
      // First player is White, second is Black
      val color = if(players.isEmpty) White else Black

      players = players + ( color -> player)

      player ! NotifyController(color, self)
      player ! GameUpdated(currentGame)
    }
    case RegisterPlayer(player) if (players.size >= 2) => {
      player ! GameFull
    }
    case RegisterListener(listener) => {
      listeners = listeners + listener
      listener ! GameUpdated(currentGame)
    }
    case PlayPos(color, pos) if (!game.isFinished()) => {
      if (color == currentGame.nextColor && sender == players(color)) {
        currentGame = currentGame.play(pos)
        (players.values ++ listeners).foreach {
          _ ! GameUpdated(currentGame)
        }
      }
    }
    case PlayPos(color, pos) if (game.isFinished()) => {
      sender ! GameFinished
    }
  }

}

object GameController {
  def initListener(gc: ActorRef) = {
    val (enumerator, channel) = Concurrent.broadcast[JsValue]
    val listenerActor = Akka.system.actorOf(Props {
      new Actor {
        def receive = {
          case GameUpdated(game) => channel.push(toJson(game))
        }
      }
    })
    gc ! RegisterListener(listenerActor)
    enumerator
  }

  def initPlayer(gc: ActorRef): (Iteratee[JsValue,_], Enumerator[JsValue]) = {
    val (enumerator, channel) = Concurrent.broadcast[JsValue]
    val humanPlayer = Akka.system.actorOf(Props{new HumanPlayer(channel)});
    val iteratee = Iteratee.foreach[JsValue]{ value =>
      humanPlayer ! UserEvent(value)
    }
    gc ! RegisterPlayer(humanPlayer)
    (iteratee, enumerator)
  }

  def addBotPlayer(gc: ActorRef) = {
    val botPlayer = Akka.system.actorOf(Props[BotPlayer])
    gc ! RegisterPlayer(botPlayer)
  }
}

case class GameUpdated(game: Game)
case object GameFull
case object GameFinished
case class RegisterPlayer(player: ActorRef)
case class RegisterListener(listener: ActorRef)
case class NotifyController(color: Color, controller: ActorRef)
case class PlayPos(color: Color, pos: Pos)
case class UserEvent(value: JsValue)
