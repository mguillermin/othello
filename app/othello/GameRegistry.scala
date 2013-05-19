package othello

import akka.actor.{ActorRef, Props, Actor}
import play.api.libs.concurrent.Akka
import play.api.Play.current
import java.util.{Date, UUID}

case class GameInfo(gameController: ActorRef, created: Date)

/**
 * The Game Registry is keeping track of running games
 */
object GameRegistry {

  var gameControllers: Map[String, GameInfo] = Map( generateId -> createGameController)

  def all: Map[String, GameInfo] = gameControllers

  def create: GameInfo = {
    val gameController = createGameController
    gameControllers = gameControllers + (generateId -> gameController)
    gameController
  }

  def get(id: String): Option[GameInfo] = gameControllers.get(id)

  private def createGameController = {
    GameInfo(Akka.system.actorOf(Props { new GameController(Game()) } ), new Date)
  }

  private def generateId = UUID.randomUUID().toString


}