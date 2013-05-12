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

  def gameSocket = WebSocket.using[JsValue] { request =>
    val controller = Akka.system.actorOf(Props { new GameController(Game()) } )
    val botPlayer = Akka.system.actorOf(Props { new BotPlayer(Black, controller) } )
    val (humanPlayer, iteratee, enumerator) = HumanPlayer.init(White, controller)

    controller ! RegisterPlayer(White, humanPlayer)
    controller ! RegisterPlayer(Black, botPlayer)

    (iteratee, enumerator)
  }

  def jsRoutes = Action { implicit request =>
    Ok(Routes.javascriptRouter("jsRoutes")(
      controllers.routes.javascript.Application.game,
      controllers.routes.javascript.Application.move,
      controllers.routes.javascript.Application.gameSocket
    )).as("text/javascript")
  }
}