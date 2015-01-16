import Chess._

import scala.annotation.tailrec
import scala.concurrent.duration._

object ChessProblem {

  def solve(chess: Chess, pieces: List[(Piece, Int)]): List[Chess] = {
    @tailrec
    def loopPossiblePieceSquares(chess: Chess, piece: Piece, acc: List[(Chess, Square)], squares: List[Square]): List[(Chess, Square)] =
      if (squares.isEmpty) acc
      else {
        val square = squares.head
        loopPossiblePieceSquares(chess, piece,
          (chess.addPiece(piece, square), square) :: acc, squares.tail)
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
        loopPossiblePiecesSquares(pieces.tail,
          acc.par.map(possibleSimilarPiecesSquares(_, piece, count)).flatten.toList)
      }

    if (pieces.isEmpty) List()
    else {
      val (piece, count) = pieces.head
      loopPossiblePiecesSquares(pieces.tail,
        possibleSimilarPiecesSquares(chess, piece, count))

    }
  }

  def main(args: Array[String]): Unit = {
    println("Solving the problem: 7×7 board with 2 Kings, 2 Queens, 2 Bishops and 1 Knight")
    val start = System.currentTimeMillis()
    val solution = solve(Chess(7, 7), List((King, 2), (Queen, 2), (Bishop, 2), (Knight, 1)))
    val end = System.currentTimeMillis()
    val elapsed = DurationLong(end - start).millis
    println("Ok, done.")
    println(f"Elapsed time ${elapsed.toMinutes} min, ${elapsed.toSeconds % 60} sec, ${elapsed.toMillis % 1000} millis (total ${elapsed.toMillis} millis)")
    println(f"I have ${solution.size} solutions. No more 3 of them follow: ")
    solution.take(3).map(println)
  }

}
