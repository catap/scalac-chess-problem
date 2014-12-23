import Chess._

import scala.annotation.tailrec
import scala.concurrent.duration._

object ChessProblem {

  def solve(board: Board, pieces: List[Piece]): List[Chess] = {
    @tailrec
    def loopPossiblePieceSquares(chess: Chess, piece: Piece, acc: List[(Chess, Square)], squares: List[Square]): List[(Chess, Square)] =
      if (squares.isEmpty) acc
      else {
        val square = squares.head
        loopPossiblePieceSquares(chess, piece,
          acc ++ List((chess.addPiece(Position(piece, square)), square)), squares.drop(1))
      }

    def possibleSimilarPiecesSquares(chess: Chess, piece: Piece, count: Int) = {
      @tailrec
      def loopPossibleSimilarPiecesSquares(count: Int, acc: List[(Chess, Square)]): List[Chess] = {
        if (count == 0) acc.map(_._1)
        else {
          loopPossibleSimilarPiecesSquares(count - 1,
            acc.foldLeft(List[(Chess, Square)]()) {
              case (list, (chess, square)) =>
                loopPossiblePieceSquares(chess, piece, list, chess.safeSquaresAfter(piece, square))
            })
        }
      }
      loopPossibleSimilarPiecesSquares(count - 1,
        loopPossiblePieceSquares(chess, piece, List(), chess.safeSquaresFor(piece)))
    }

    @tailrec
    def loopPossiblePiecesSquares(pieces: List[(Piece, Int)], acc: List[Chess]): List[Chess] =
      if (pieces.isEmpty) acc
      else {
        val (piece, count) = pieces.head
        loopPossiblePiecesSquares(pieces.drop(1),
          acc.map(possibleSimilarPiecesSquares(_, piece, count)).flatten)
      }

    if (pieces.isEmpty) List()
    else {
      val groupedPieces = pieces.groupBy(p => p).mapValues(_.size).toList
      val (piece, count) = groupedPieces.head
      loopPossiblePiecesSquares(groupedPieces.drop(1),
        possibleSimilarPiecesSquares(Chess(board, Set()), piece, count))

    }
  }

  def main(args: Array[String]): Unit = {
    println("Solving the problem: 7×7 board with 2 Kings, 2 Queens, 2 Bishops and 1 Knight")
    val start = System.currentTimeMillis()
    val solution = solve(Board(7, 7), List(King, King, Queen, Queen, Bishop, Bishop, Knight))
    val end = System.currentTimeMillis()
    val elapsed = DurationLong(end - start).millis
    println("Ok, done.")
    println(f"Elapsed time ${elapsed.toMinutes} min, ${elapsed.toSeconds % 60} sec, ${elapsed.toMillis % 1000} millis (total ${elapsed.toMillis} millis)")
    println(f"I have ${solution.size} solutions. No more of 3 of them follow: ")
    solution.take(3).map(println)
  }

}
