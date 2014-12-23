import Chess._

import scala.annotation.tailrec

object ChessProblem {

  def solve(board: Board, pieces: List[Piece]): List[Chess] = {
    @tailrec
    def loopPossiblePieceSquares(chess: Chess, piece: Piece, acc: List[(Chess, Square)], squares: List[Square]): List[(Chess, Square)] =
      if (squares.isEmpty) acc
      else {
        val square = squares.head
        val _acc =
          if (chess.occupied.exists(_.square == square)) acc
          else {
            val likelyChess = chess.addPiece(Position(piece, square))
            if (likelyChess.isThreaten) acc
            else acc ++ List((likelyChess, square))
          }
        loopPossiblePieceSquares(chess, piece, _acc, squares.drop(1))
      }

    def possibleSimilarPiecesSquares(chess: Chess, piece: Piece, count: Int) = {
      @tailrec
      def loopPossibleSimilarPiecesSquares(count: Int, acc: List[(Chess, Square)]): List[Chess] = {
        if (count == 0) acc.map(_._1)
        else {
          loopPossibleSimilarPiecesSquares(count - 1, acc.map {
            case (chess, square) =>
              loopPossiblePieceSquares(chess, piece, List(), chess.board.after(square))
          }.flatten)
        }
      }
      loopPossibleSimilarPiecesSquares(count - 1,
        loopPossiblePieceSquares(chess, piece, List(), chess.board.full))
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
    println("Input: 3×3 board containing 2 Kings and 1 Rook")
    val solution = solve(Chess.Board(3, 3), List(Chess.King, Chess.King, Chess.Rock))
    println(f"I have ${solution.size} solutions. No more of 3 of them follow: ")
    solution.take(3).map(println)
  }

}
