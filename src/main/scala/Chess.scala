object Chess {

  case class Square(x: Int, y: Int)

  case class Board(x: Int, y: Int)

  sealed trait Piece {
    def possiblePositions(current: Square, board: Board): Set[Square]

    def isThreatens(current: Square, board: Board, targets: Set[Square]): Boolean = {
      possiblePositions(current, board)
        .filter(targets.contains)
        .nonEmpty
    }
  }

  case object King extends Piece {
    def possiblePositions(current: Square, board: Board): Set[Square] =
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