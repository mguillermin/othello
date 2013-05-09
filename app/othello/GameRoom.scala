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
import akka.util.Timeout
import akka.pattern.ask
import java.util.UUID
import play.api.libs.iteratee.Concurrent.Channel

class GameRoom extends Actor {
  var games = Map[String, Game]()
  var channels = Map[String, Channel[JsValue]]()

  def receive = {
    case Join => {
      val id = UUID.randomUUID().toString
      games = games + (id -> Game())
      val (enumerator, channel) = Concurrent.broadcast[JsValue]
      channels = channels + (id -> channel)
      sender ! Connected(id, enumerator)
      channels(id).push(toJson(games(id)))
    }
    case Move(id, value) => {
      Json.fromJson[Pos](value).map { pos =>
        games = games.updated(id, games(id).play(pos))
        channels(id).push(toJson(games(id)))
      }
    }
  }
}

object GameRoom {
  implicit val timeout = Timeout(1 seconds)
  lazy val gameActor: ActorRef = Akka.system.actorOf(Props[GameRoom])

  def join(): Future[(Iteratee[JsValue,_], Enumerator[JsValue])] = {
    (gameActor ? Join).map {
      case Connected(id, enumerator) =>
        val iteratee = Iteratee.foreach[JsValue] { event =>
          gameActor ! Move(id, event)
        }.mapDone { _ =>
          gameActor ! Quit(id)
        }
        (iteratee, enumerator)
    }
  }
}

case object Join
case class Connected(id: String, enumerator: Enumerator[JsValue])
case class Move(id: String, value: JsValue)
case class Quit(id: String)