object Chess {

  case class Position(x: Int, y: Int)

  case class Board(x: Int, y: Int)

  sealed trait Piece {
    def possiblePositions(current: Position, board: Board): Set[Position]

    def isThreatens(current: Position, board: Board, targets: Set[Position]): Boolean = {
      possiblePositions(current, board)
        .filter(targets.contains)
        .nonEmpty
    }
  }

  case object King extends Piece {
    def possiblePositions(current: Position, board: Board): Set[Position] =
      (-1 to 1).map(x =>
        (-1 to 1).map(y =>
          Position(current.x + x, current.y + y)
        ))
        .flatten
        .filter(pos => pos != current)
        .filter(pos => pos.x >= 0 && pos.y >= 0)
        .filter(pos => pos.x <= board.x && pos.y <= board.y)
        .toSet
  }


}