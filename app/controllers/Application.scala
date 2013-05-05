package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import othello.{Pos, Color, Game}
import play.api.libs.json.Json

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
    //TODO: Check if there is possible moves for this color
    // and check if the current move is possible
    // if this is not the case, return an Invalid Response
    moveForm.bindFromRequest.fold({
      formWithErrors =>
        BadRequest("Error")
      },{
      value =>
        val (row, col, color) = value
        g = g.play(Pos(row, col))
        Ok(Json.toJson(g))
      }
    )
  }

  def jsRoutes = Action { implicit request =>
    Ok(Routes.javascriptRouter("jsRoutes")(
      controllers.routes.javascript.Application.game,
      controllers.routes.javascript.Application.move
    )).as("text/javascript")
  }
}