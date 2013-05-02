package othello

sealed trait Color {
  def invert: Color
}
case object Black extends Color {
  def invert = White
}
case object White extends Color {
  def invert = Black
}

object Color {
  def apply(c: Char): Option[Color] = c match {
    case 'W' => Some(White)
    case 'B' => Some(Black)
    case _ => None
  }
}

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction
case object UpLeft extends Direction
case object UpRight extends Direction
case object DownLeft extends Direction
case object DownRight extends Direction
object Direction {
  def all = Set(Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight)
}

case class Pos(row: Int, col: Int) {
  def move(direction: Direction) = direction match {
    case Up => Pos(row - 1, col)
    case Down => Pos(row + 1, col)
    case Left => Pos(row, col - 1)
    case Right => Pos(row, col + 1)
    case UpLeft => Pos(row - 1, col - 1)
    case UpRight => Pos(row - 1, col + 1)
    case DownLeft => Pos(row + 1, col - 1)
    case DownRight => Pos(row + 1, col + 1)
  }

  def isValid(): Boolean = (row >= 0 && row <= 7 && col >=0 && col <= 7)
}

case class Disc(color: Color, pos: Pos)

case class Board(repr: Map[Pos, Color] = Map()) {
  def update(pos: Pos, color: Color): Board = copy(repr = repr.updated(pos, color))

  def isColor(pos: Pos, color: Color) =
    get(pos) match {
      case None => false
      case Some(c) => c == color
    }

  def isEmpty(pos: Pos) = get(pos) == None

  def get(pos: Pos): Option[Color] = repr.get(pos)

  def isEmpty = repr.isEmpty

  def count(color: Color) = repr.count{ case (p, c) => c == color }

  override def toString = {
    (for (i <- 0 to 7) yield {
      (for (j <- 0 to 7) yield {
        repr.get(Pos(i,j)) match {
          case Some(White) => "W"
          case Some(Black) => "B"
          case _ => "-"
        }
      }).mkString(" ")
    }).mkString("\n")
  }

  def possibleMoves(color: Color): Set[(Pos, Set[Pos])] =
    {
      for {
        i <- 0 to 7
        j <- 0 to 7
        winningPositions <- Some(winningPositions(Pos(i,j), color))
        if (winningPositions.size > 0)
      } yield (Pos(i, j), winningPositions)
    }.toSet


  def bestMoves(color: Color): Set[(Pos, Set[Pos])] = {
    val availableMoves = possibleMoves(color)
    if (availableMoves.isEmpty) {
      availableMoves
    } else {
      val maxScore = availableMoves.map(_._2.size).max
      availableMoves.filter { case (_, wins) => wins.size == maxScore }
    }
  }

  def winningPositions(pos: Pos, color: Color): Set[Pos] =
    for {
      dir <- Direction.all
      winningPos <- winningPositions(pos, color, dir)
    } yield winningPos

  def winningPositions(pos: Pos, color: Color, dir: Direction): List[Pos] = {
    val followList = followWithPos(pos, dir)
    followList match {
      case (_,None) :: followers => {
        otherColorsUntilMe(followers, color)
      }
      case _ => Nil
    }
  }

  def otherColorsUntilMe(followers: List[(Pos, Option[Color])], color: Color): List[Pos] = {
    // If color is not in the followers list
    if (!followers.map(_._2).contains(Some(color))) {
      Nil
    } else {
      // Taking all elements of the different color
      val otherColors = followers.takeWhile(_._2 != Some(color))
      // If there is some "None" in this list, then it's not a valid
      if (otherColors.map(_._2).contains(None)) {
        Nil
      } else {
        // We return all the positions
        otherColors.map(_._1)
      }
    }
  }

  def winningPositionsOld(pos: Pos, color: Color, dir: Direction): List[Pos] = {
    val followList = followWithPos(pos, dir)
    followList match {
      case (_,None) :: followers => {
        followers.headOption match {
          case Some((_,Some(c))) if c != color && followers.exists(_._2 == Some(color)) => {
            val f2 = followers.takeWhile( _._2 != Some(color))
            if (f2.exists (_._2 == None)) {
              Nil
            } else {
              f2.map(_._1)
            }
          }
          case _ => Nil
        }
      }
      case _ => Nil
    }
  }


  def follow(pos: Pos, dir: Direction): List[Option[Color]] = {
    if (!pos.isValid())
      Nil
    else
      get(pos) :: follow(pos.move(dir), dir)
  }
  def followWithPos(pos: Pos, dir: Direction): List[(Pos, Option[Color])] = {
    if (!pos.isValid())
      Nil
    else
      (pos, get(pos)) :: followWithPos(pos.move(dir), dir)
  }


}

object Board {
  /**
   * Construct a Board with the initial state (2 Blacks / 2 Whites in the center of the board)
   * @return
   */
  def initialState = Board(Map(
    Pos(3, 3) -> White,
    Pos(3, 4) -> Black,
    Pos(4, 3) -> Black,
    Pos(4, 4) -> White
  ))

  def fromString(board: String) : Board = Board({
    val rows = board.split("\n").zipWithIndex
    (
      for {
        (row, i) <- rows
        (c, j) <- row.zipWithIndex
        color <- Color(c)
      } yield (Pos(i, j), color)
    ).toMap
  })
}
