package othello

import scala.util.Random

/**
 * An Othello Game
 */
class Game {

  var boards: List[Board] = Nil
  var nextColor: Color = White
  var noMoves = 0

  def start() = {
    val board = Board.initialState
    boards = board :: boards
  }

  def nextStep() = {
    val b = boards.head
    val moves = b.bestMoves(nextColor)
    if (moves.isEmpty) {
      noMoves = noMoves + 1
      boards = b :: boards
      nextColor = nextColor.invert
    } else {
      noMoves = 0
      val (score, nextPos) = moves(Random.nextInt(moves.size))
      val winningPositions = b.winningPositions(nextPos, nextColor)
      val newBoard = winningPositions.foldLeft(b)((board, pos) => board.update(pos, nextColor))
      boards = newBoard.update(nextPos, nextColor) :: boards
      nextColor = nextColor.invert
    }
  }

  override def toString = boards match {
    case head :: _ => head.toString
    case Nil => ""
  }
}
