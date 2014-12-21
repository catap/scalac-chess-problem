

case class Chess(board: Chess.Board, occupied: Set[Chess.Position]) {
  lazy val occupiedSquare = occupied.map(_.square)
  lazy val isThreaten =
    occupied.filter(position =>
      position.piece.isThreatens(position.square, board, occupiedSquare))
      .nonEmpty

  lazy val threatenedSquare =
    occupied.map(position => position.piece.possibleSquares(position.square, board))
      .flatten

  override def toString: String =
    (0 to board.y).map(y =>
      (0 to board.x).map(x =>
        occupied.find {
          case Chess.Position(_, Chess.Square(_x, _y)) if _x == x && _y == y => true
          case _ => false
        }.map(position => position.piece.toString)
          .getOrElse(
            if (threatenedSquare.exists(square => square.x == x && square.y == y)) "*"
            else "-"
          ) + (if (x == board.x) "\n" else "")
      ))
      .flatten
      .mkString
}

object Chess {

  case class Square(x: Int, y: Int)

  case class Position(piece: Piece, square: Square)

  case class Board(x: Int, y: Int)

  sealed trait Piece {
    def possibleSquares(current: Square, board: Board): Set[Square]

    def isThreatens(current: Square, board: Board, targets: Set[Square]): Boolean = {
      possibleSquares(current, board)
        .filter(targets.contains)
        .nonEmpty
    }
  }

  case object King extends Piece {

    override def toString: String = "K"

    def possibleSquares(current: Square, board: Board): Set[Square] =
      (-1 to 1).map(x =>
        (-1 to 1).map(y =>
          Square(current.x + x, current.y + y)
        ))
        .flatten
        .filter(pos => pos != current)
        .filter(pos => pos.x >= 0 && pos.y >= 0)
        .filter(pos => pos.x <= board.x && pos.y <= board.y)
        .toSet
  }

}