import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import othello.{Pos, Black, White, Board}

/**
 * Specifications of the Game
 */
class GameSpec extends FunSpec with MustMatchers {
  describe("A newly created Board") {
    it("should be empty") {
      val board = Board()
      board.isEmpty must be(true)
    }

    it("should allow to insert a new color") {
      val board = Board()
      val updatedBoard = board.update(Pos(2, 3), White)
      updatedBoard.get(Pos(2,3)) must be(Some(White))
    }

    it("should be equal to a board with the same colors at the same positions") {
      val board = Board().update(Pos(2, 3), White).update(Pos(4, 5), Black)
      val board2 = Board().update(Pos(2, 3), White).update(Pos(4, 5), Black)
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

  describe("fromString constructor") {
    it("should correctly parse an empty board string") {
      val b =
        """--------
          |--------
          |--------
          |--------
          |--------
          |--------
          |--------
          |--------
        """.stripMargin
      Board.fromString(b).isEmpty must be(true)
    }
    it("should correctly parse a board string with some colors") {
      val b =
        """--------
          |--------
          |--------
          |-W------
          |--------
          |------B-
          |--------
          |--------
        """.stripMargin
      val board = Board.fromString(b)
      board.isEmpty must be(false)
      board.get(Pos(3, 1)) must be(Some(White))
      board.get(Pos(5, 6)) must be(Some(Black))
    }
  }

  describe("possible moves") {
    it("should be found correctly") {
      val b =
        """--W-----
          |--W-----
          |--B-----
          |---W----
          |---B----
          |----BW--
          |-----W--
          |--------
        """.stripMargin
      val board = Board.fromString(b)
      val blackMoves: Set[(Pos, Set[Pos])] = board.possibleMoves(Black)
      blackMoves must be(Set(
        (Pos(2,3), Set(Pos(3,3))),
        (Pos(5,6), Set(Pos(5,5))),
        (Pos(4,4), Set(Pos(3,3))),
        (Pos(7,6), Set(Pos(6,5)))
      ))
      val whiteMoves: Set[(Pos, Set[Pos])] = board.possibleMoves(White)
      whiteMoves must be(Set(
        (Pos(1,1), Set(Pos(2,2))),
        (Pos(3,2), Set(Pos(2,2), Pos(4,3), Pos(5,4))),
        (Pos(5,3), Set(Pos(4,3), Pos(5,4)))
      ))
    }
  }

  describe("best moves") {
    it("should be found correctly") {
      val b =
        """--W-----
          |--W-----
          |--B-----
          |---W----
          |---B----
          |----BW--
          |-----W--
          |--------
        """.stripMargin
      val board = Board.fromString(b)
      val bestWhiteMoves: Set[(Pos, Set[Pos])] = board.bestMoves(White)
      bestWhiteMoves must be(Set(
        (Pos(3,2), Set(Pos(2,2), Pos(4,3), Pos(5,4)))
      ))
    }
  }

}
