
case class Chess(board: Chess.Board, occupied: Set[Chess.Position]) {
  lazy val occupiedSquare = occupied.map(_.square)
  lazy val isThreaten =
    occupied.filter(position =>
      position.piece.isThreatens(position.square, board, occupiedSquare))
      .nonEmpty
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