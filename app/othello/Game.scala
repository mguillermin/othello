package othello

import scala.util.Random
import play.api.libs.json._
import Json.toJson
import play.api.libs.json.JsString


/**
 * An Othello Game
 */
case class Game(board: Board = Board.initialState, nextColor: Color = White) {

  def isFinished(): Boolean = board.isFull ||
    (board.possibleMoves(nextColor).isEmpty && board.possibleMoves(nextColor.invert).isEmpty)

  def randomPlay: Game = {
    val moves: Set[(Pos, Set[Pos])] = board.possibleMoves(nextColor)
    if (moves.isEmpty) {
      copy(nextColor = nextColor.invert)
    } else {
      val (randomPos,_) = moves.toVector(Random.nextInt(moves.size))
      play(randomPos)
    }
  }

  def isMoveValid(pos: Pos): Boolean =
    !board.winningPositions(pos, nextColor).isEmpty

  def play(pos: Pos): Game = {
    val wins = board.winningPositions(pos, nextColor)
    val updatedBoard: Board = board.update(pos, nextColor).updateAll(wins, nextColor)
    // If there is no possible move for the next color, keep the current color
    if (updatedBoard.possibleMoves(nextColor.invert).isEmpty) {
      copy(board = updatedBoard)
    } else {
      copy(board = updatedBoard, nextColor = nextColor.invert)
    }
  }

  def score: Map[Color, Int] =
    Map(
      White -> board.count(White),
      Black -> board.count(Black)
    )

  override def toString = {
    "Board:\n" + board + "\n" + "Next color: " + nextColor
  }
}

object Game {
  def simulGame(board: Board = Board.initialState) {
    var g = Game(board = board)
    while (!g.isFinished()) {
      g = g.randomPlay
      println("*********")
      println(g)
      println(g.score)
    }
  }


  implicit val posReads: Reads[Pos] = new Reads[Pos] {
    def reads(json: JsValue) = JsSuccess(Pos(json(0).as[Int],json(1).as[Int]))
  }

  implicit val posWrites: Writes[Pos] = new Writes[Pos] {
    def writes(p: Pos) = toJson(Seq(p.row, p.col))
  }

  implicit val colorWrites: Writes[Color] = new Writes[Color] {
    def writes(o: Color) = o match {
      case c if (c == White) => JsString("White")
      case c if (c == Black) => JsString("Black")
    }
  }

  implicit val posColorWrites: Writes[(Pos, Color)] = new Writes[(Pos, Color)] {
    def writes(o: (Pos, Color)) = o match {
      case (pos, color) => toJson(Seq(toJson(pos.row), toJson(pos.col), toJson(color)))
    }
  }

  implicit val boardWrites: Writes[Board] = new Writes[Board] {
    def writes(b: Board) = toJson(
      b.repr.toList
    )
  }

  implicit val gameWrites: Writes[Game] = new Writes[Game]{
    def writes(g: Game): JsValue = {
      Json.obj(
        "board" -> toJson(g.board),
        "nextColor" -> toJson(g.nextColor),
        "possibleMoves" -> toJson(g.board.possibleMoves(g.nextColor).map(m => toJson(m._1) )),
        "score" -> toJson(g.score.map { case (color, score) => Map(color.toString ->toJson(score))})
      )
    }
  }
}
