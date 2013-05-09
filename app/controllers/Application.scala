package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import othello.{GameRoom, Pos, Color, Game}
import play.api.libs.json.{JsValue, Json}
import play.api.libs.iteratee.Iteratee

object Application extends Controller {

  //TODO: do not rely on this val
  var g: Game = Game()

  def index = Action {
    Ok(views.html.index())
  }

  def game = Action {
    Ok(Json.toJson(g));
  }

  val moveForm = Form(
    tuple(
      "row" -> number,
      "col" -> number,
      "color" -> text
    )
  )

  def move = Action { implicit request =>
    moveForm.bindFromRequest.fold({
      formWithErrors =>
        BadRequest("Error")
      },{
      value =>
        val (row, col, color) = value
        if (g.isMoveValid(Pos(row, col))) {
          g = g.play(Pos(row, col))
          Ok(Json.toJson(g))
        } else {
          BadRequest("Invalid move")
        }
      }
    )
  }

  def gameSocket = WebSocket.async[JsValue] { request =>
    GameRoom.join()
  }

  def jsRoutes = Action { implicit request =>
    Ok(Routes.javascriptRouter("jsRoutes")(
      controllers.routes.javascript.Application.game,
      controllers.routes.javascript.Application.move
    )).as("text/javascript")
  }
}