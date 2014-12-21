import scala.annotation.tailrec

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
    def possibleSquaresFilter(current: Square, x: Int, y: Int): Boolean

    private def testSquare(current: Square, x: Int, y: Int) =
      if (x == current.x && y == current.y)
        false
      else {
        if (possibleSquaresFilter(current, x, y)) true
        else false
      }

    @tailrec
    private final def loopTestSquare(current: Square, board: Board, x: Int, y: Int, acc: Set[Square]): Set[Square] = {
      val acc_new =
        if (testSquare(current, x, y)) acc ++ Set(Square(x, y))
        else acc
      (x, y) match {
        case (board.x, board.y) =>
          acc_new
        case (_, board.y) =>
          loopTestSquare(current, board, x + 1, 1, acc_new)
        case (_, _) =>
          loopTestSquare(current, board, x, y + 1, acc_new)
      }
    }

    def possibleSquares(current: Square, board: Board): Set[Square] = {
      loopTestSquare(current, board, 1, 1, Set())
    }

    @tailrec
    private final def loopTestThreatens(current: Square, board: Board, targets: Set[Square], x: Int, y: Int, threaten: Boolean): Boolean = {
      val acc_new =
        if (testSquare(current, x, y))
          if (targets.contains(Square(x, y)))
            true
          else false
        else false
      (acc_new, x, y) match {
        case (true, _, _) => true
        case (false, board.x, board.y) =>
          acc_new
        case (false, _, board.y) =>
          loopTestThreatens(current, board, targets, x + 1, 1, acc_new)
        case (false, _, _) =>
          loopTestThreatens(current, board, targets, x, y + 1, acc_new)
      }
    }

    def isThreatens(current: Square, board: Board, targets: Set[Square]): Boolean =
      loopTestThreatens(current, board, targets, 1, 1, false)
  }

  case object King extends Piece {
    override def toString: String = "♔"

    override def possibleSquaresFilter(current: Square, x: Int, y: Int): Boolean =
      Math.abs(current.x - x) <= 1 && Math.abs(current.y - y) <= 1
  }

  case object Queen extends Piece {
    override def toString: String = "♕"

    override def possibleSquaresFilter(current: Square, x: Int, y: Int): Boolean =
      current.x == x || current.y == y ||
        Math.abs(current.x - x) == Math.abs(current.y - y)
  }

  case object Rock extends Piece {
    override def toString: String = "♖"

    override def possibleSquaresFilter(current: Square, x: Int, y: Int): Boolean =
      current.x == x || current.y == y
  }

  case object Bishop extends Piece {
    override def toString: String = "♗"

    override def possibleSquaresFilter(current: Square, x: Int, y: Int): Boolean =
      Math.abs(current.x - x) == Math.abs(current.y - y)
  }

  case object Knight extends Piece {
    override def toString: String = "♘"

    override def possibleSquaresFilter(current: Square, x: Int, y: Int): Boolean =
      (Math.abs(current.x - x) == 1 && Math.abs(current.y - y) == 2) ||
        (Math.abs(current.x - x) == 2 && Math.abs(current.y - y) == 1)
  }

}