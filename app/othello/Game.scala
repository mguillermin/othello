package othello

import scala.util.Random
import play.api.libs.json.{JsString, JsValue, Writes, Json}
import Json.toJson


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

  def play(pos: Pos): Game = {
    val possibleMoves: Map[Pos, Set[Pos]] = board.possibleMoves(nextColor).toMap
    val updatedBoard: Board = possibleMoves.get(pos).foldLeft(board){ (b, wins) =>
      b.update(pos, nextColor).updateAll(wins, nextColor)
    }
    copy(board = updatedBoard, nextColor = nextColor.invert)
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
      case (pos, color) => toJson(Map(
        "pos" -> toJson(pos),
        "color" -> toJson(color)
      ))
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
        "score" -> toJson(g.score.map { case (color, score) => Map(color.toString ->toJson(score))})
      )
    }
  }
}
