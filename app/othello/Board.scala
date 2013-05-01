package othello

sealed trait Color
case object Black extends Color
case object White extends Color

case class Board(val rows: Vector[Vector[Option[Color]]] = Vector.fill(8,8)(None)) {
  def update(row: Int, col: Int, color: Option[Color]): Board = {
    copy(rows = rows.updated(row, rows(row).updated(col, color)))
  }

  def isEmpty = rows.forall(_.forall(_ == None))

  def count(color: Color) = rows.map { row =>
    row.filter(_ == Some(color)).size
  }.sum

  override def toString = {
    rows.map { row =>
      row.map {
        case None => "-"
        case Some(Black) => "B"
        case Some(White) => "W"
      }.mkString
    }.mkString("\n")
  }
}

object Board {
  /**
   * Construct a Board with the initial state (2 Blacks / 2 Whites in the center of the board)
   * @return
   */
  def initialState =
    Board()
      .update(3, 3, Some(White))
      .update(3, 4, Some(Black))
      .update(4, 3, Some(Black))
      .update(4, 4, Some(White))
}