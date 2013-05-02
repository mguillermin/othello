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
object Direction {
  def all = Seq(Up, Down, Left, Right)
}

case class Pos(row: Int, col: Int) {
  def move(direction: Direction) = direction match {
    case Up => Pos(row - 1, col)
    case Down => Pos(row + 1, col)
    case Left => Pos(row, col - 1)
    case Right => Pos(row, col + 1)
  }

  def isValid(): Boolean = (row >= 0 && row <= 7 && col >=0 && col <= 7)
}

case class Disc(color: Color, pos: Pos)

case class Board(repr: Set[Disc] = Set()) {
  def update(disc: Disc): Board = copy(repr = repr + disc)

  def update(pos: Pos, color: Color): Board = update(Disc(color, pos))

  def isColor(pos: Pos, color: Color) =
    get(pos) match {
      case None => false
      case Some(c) => c == color
    }

  def isEmpty(pos: Pos) = get(pos) == None

  def get(pos: Pos): Option[Color] = repr.find(_.pos == pos).map(_.color)

  def getAll(color: Color) = repr.filter(_.color == color)

  def isEmpty = repr.isEmpty

  def count(color: Color) = repr.count(_.color == color)

  override def toString = {
    (for (i <- 0 to 7) yield {
      (for (j <- 0 to 7) yield {
        repr.find(d => d.pos.row == i && d.pos.col == j) match {
          case Some(Disc(White, _)) => "W"
          case Some(Disc(Black, _)) => "B"
          case _ => "-"
        }
      }).mkString(" ")
    }).mkString("\n")
  }

  def posScores(pos: Pos, color: Color) = {
    Direction.all.map(d => (d, moveScore(pos, color, d))).toMap
  }

  def moveScore(pos: Pos, color: Color, dir: Direction): Int = {
    if (pos.isValid() && isEmpty(pos)) {
      val followList = follow(pos.move(dir), dir)
      if (followList.isEmpty) {
        0
      } else {
        followList.collect { case Some(c) => c }.span(_ != color)._1.size
      }
    } else {
      0
    }
  }

  def follow(pos: Pos, dir: Direction): List[Option[Color]] = {
    if (!pos.isValid())
      Nil
    else
      get(pos) :: follow(pos.move(dir), dir)
  }


}

object Board {
  /**
   * Construct a Board with the initial state (2 Blacks / 2 Whites in the center of the board)
   * @return
   */
  def initialState = Board(Set(
    Disc(White, Pos(3, 3)),
    Disc(Black, Pos(3, 4)),
    Disc(Black, Pos(4, 3)),
    Disc(White, Pos(4, 4))
  ))

  def formString(board: String) : Board = Board({
    val rows = board.split("\n").zipWithIndex
    (
      for {
        (row, i) <- rows
        (c, j) <- row.zipWithIndex
        color <- Color(c)
      } yield Disc(color, Pos(i, j))
    ).toSet[Disc]
  })
}
