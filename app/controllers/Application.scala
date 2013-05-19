package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import othello._
import play.api.libs.json.{JsValue, Json}
import play.api.libs.iteratee.{Concurrent, Iteratee}
import othello.Pos
import play.api.libs.concurrent.Akka
import akka.actor.Props
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._


object Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  def jsRoutes = Action { implicit request =>
    Ok(Routes.javascriptRouter("jsRoutes")(
      controllers.routes.javascript.Games.listenerSocket,
      controllers.routes.javascript.Games.playerSocket,
      controllers.routes.javascript.Games.addBot
    )).as("text/javascript")
  }
}