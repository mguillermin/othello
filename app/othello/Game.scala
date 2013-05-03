package othello

import scala.util.Random

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
}
