import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import othello.{Black, White, Board}

/**
 * Created with IntelliJ IDEA.
 * User: mguillermin
 * Date: 01/05/13
 * Time: 13:17
 * To change this template use File | Settings | File Templates.
 */
class GameSpec extends FunSpec with MustMatchers {
  describe("A newly created Board") {
    it("should be empty") {
      val board = Board()
      board.isEmpty must be(true)
    }

    it("should allow to insert a new color") {
      val board = Board()
      val updatedBoard = board.update(2, 3, Some(White))
      updatedBoard.rows(2)(3) must be(Some(White))
    }

    it("should allow to remove a color") {
      val board = Board().update(2, 3, Some(White))
      val updatedBoard = board.update(2, 3, None)
      updatedBoard.isEmpty must be(true)
    }

    it("should be equal to a board with the same colors at the same positions") {
      val board = Board().update(2, 3, Some(White)).update(4, 5, Some(Black))
      val board2 = Board().update(2, 3, Some(White)).update(4, 5, Some(Black))
      board2 must be equals(board)
    }
  }

  describe("Board initial state") {
    it("should contain 2 Black and 2 White") {
      val board = Board.initialState
      board.count(White) must be(2)
      board.count(Black) must be(2)
    }
  }

}
