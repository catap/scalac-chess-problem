import Chess._

import scala.annotation.tailrec

case class Chess(board: Board, occupied: List[Position], occupiedSquare: List[Square], safeSquares: List[Square]) {
  lazy val isThreaten =
    occupied.filter(position =>
      position.piece.isThreatens(position.square, board, occupiedSquare))
      .nonEmpty

  def addPiece(piece: Piece, square: Square): Chess =
    copy(occupied = occupied.+:(Position(piece, square)),
      occupiedSquare = occupiedSquare.+:(square),
      safeSquares =
        safeSquares
          .filter(target =>
          !square.equals(target) && !piece.testSquare(square, target.x, target.y)))

  def addPiece(piece: Piece, x: Int, y: Int): Chess =
    addPiece(piece, Square(x, y))

  def safeSquaresAfter(piece: Piece, square: Square) =
    safeSquaresFor(piece)
      .filter(square < _)

  def safeSquaresFor(piece: Piece) =
    safeSquares
      .filter(!piece.isThreatens(_, board, occupiedSquare))

  override def toString: String =
    board.full.map(s =>
      occupied.find {
        case Position(_, Square(s.x, s.y)) => true
        case _ => false
      }.map(_.piece.toString)
        .getOrElse(
          if (!safeSquares.exists(s.equals)) "*"
          else "-"
        ) + (if (s.x == board.x) "\n" else "")
    ).mkString
}

object Chess {

  def apply(board: Board): Chess =
    Chess(board, List(), List(), board.full)

  def apply(x: Int, y: Int): Chess =
    Chess(Board(x, y))

  case class Square(x: Int, y: Int) extends Ordered[Square] {
    override def compare(that: Square): Int = {
      val r = this.y - that.y
      if (r != 0) r
      else this.x - that.x
    }
  }

  case class Position(piece: Piece, square: Square)

  case class Board(x: Int, y: Int) {
    lazy val full =
      (for (_y <- 1 to y; _x <- 1 to x) yield Square(_x, _y)).toList
  }

  sealed trait Piece {
    def possibleSquaresFilter(current: Square, x: Int, y: Int): Boolean

    def testSquare(current: Square, x: Int, y: Int) =
      if (x == current.x && y == current.y)
        false
      else {
        if (possibleSquaresFilter(current, x, y)) true
        else false
      }

    @tailrec
    private final def loopTestSquare(current: Square, board: Board, x: Int, y: Int, acc: List[Square]): List[Square] = {
      val acc_new =
        if (testSquare(current, x, y)) acc.+:(Square(x, y))
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

    def possibleSquares(current: Square, board: Board): List[Square] = {
      loopTestSquare(current, board, 1, 1, List())
    }

    def isThreatens(current: Square, board: Board, targets: List[Square]): Boolean =
      targets.exists(target => testSquare(current, target.x, target.y))
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