package controllers

import play.api.mvc.{WebSocket, Action, Controller}
import othello._
import play.api.libs.concurrent.Akka
import play.api.Play.current
import akka.actor.Props
import play.api.libs.json.JsValue
import play.api.libs.iteratee.{Enumerator, Iteratee}
import othello.RegisterPlayer
import othello.BotPlayer

/**
 * Games controller
 */
object Games extends Controller {

  def index = Action {
    Ok(views.html.games.index(GameRegistry.all))
  }

  def create = Action {
    GameRegistry.create
    Redirect(routes.Games.index())
  }

  def show(id: String) = Action {
    GameRegistry.get(id).map { gi =>
      Ok(views.html.games.show(id))
    }.getOrElse {
      NotFound
    }
  }

  def listenerSocket(id: String) = WebSocket.using[JsValue] { request =>
    GameRegistry.get(id).map { gi =>
      val listener: Enumerator[JsValue] = GameController.initListener(gi.gameController)
      (Iteratee.ignore[JsValue], listener)
    }.getOrElse {
      throw new Exception("Could not start listener")
    }
  }

  def playerSocket(id: String) = WebSocket.using[JsValue] { request =>
    GameRegistry.get(id).map { gi =>
      GameController.initPlayer(gi.gameController)
    }.getOrElse{
      throw new Exception("Could not start player")
    }
  }

  def addBot(id: String) = Action {
    GameRegistry.get(id).map { gi =>
      GameController.addBotPlayer(gi.gameController)
      Ok("Bot player added")
    }.getOrElse {
      NotFound
    }
  }
}
