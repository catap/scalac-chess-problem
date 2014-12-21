

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
    (1 to board.y).map(y =>
      (1 to board.x).map(x =>
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
    def possibleSquaresFilter(current: Square)(xy: (Int, Int)): Boolean

    def possibleSquares(current: Square, board: Board): Set[Square] =
      (for (x <- 1 to board.x; y <- 1 to board.y)
      yield (x, y))
        .filter(xy => xy._1 != current.x || xy._2 != current.y)
        .filter(possibleSquaresFilter(current))
        .map(xy => Square(xy._1, xy._2))
        .toSet

    def isThreatens(current: Square, board: Board, targets: Set[Square]): Boolean = {
      possibleSquares(current, board)
        .filter(targets.contains)
        .nonEmpty
    }
  }

  case object King extends Piece {
    override def toString: String = "♔"

    override def possibleSquaresFilter(current: Square)(xy: (Int, Int)): Boolean =
      Math.abs(current.x - xy._1) <= 1 && Math.abs(current.y - xy._2) <= 1
  }

  case object Queen extends Piece {
    override def toString: String = "♕"

    override def possibleSquaresFilter(current: Square)(xy: (Int, Int)): Boolean =
      current.x == xy._1 || current.y == xy._2 ||
        Math.abs(current.x - xy._1) == Math.abs(current.y - xy._2)
  }

  case object Rock extends Piece {
    override def toString: String = "♖"

    override def possibleSquaresFilter(current: Square)(xy: (Int, Int)): Boolean =
      current.x == xy._1 || current.y == xy._2
  }

  case object Bishop extends Piece {
    override def toString: String = "♗"

    override def possibleSquaresFilter(current: Square)(xy: (Int, Int)): Boolean =
      Math.abs(current.x - xy._1) == Math.abs(current.y - xy._2)
  }

  case object Knight extends Piece {
    override def toString: String = "♘"

    override def possibleSquaresFilter(current: Square)(xy: (Int, Int)): Boolean =
      (Math.abs(current.x - xy._1) == 1 && Math.abs(current.y - xy._2) == 2) ||
        (Math.abs(current.x - xy._1) == 2 && Math.abs(current.y - xy._2) == 1)
  }
}