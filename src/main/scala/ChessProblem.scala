import Chess._

import scala.annotation.tailrec

object ChessProblem {

  def solve(board: Board, pieces: List[Piece]) = {

    val all_pieces = pieces.permutations.toList

    @tailrec
    def loopTestPieces(squares: List[Square], pieces: List[List[Piece]], safeCombination: Set[Chess]): Set[Chess] = {
      if (pieces.isEmpty) safeCombination
      else {
        val piece = pieces.head
        val occupied = squares.zip(piece).map(p => Position(p._2, p._1)).toSet
        val chess = Chess(board, occupied)
        val newSafeCombination =
          if (!chess.isThreaten) safeCombination ++ Set(chess)
          else safeCombination
        loopTestPieces(squares, pieces.drop(1), newSafeCombination)
      }
    }

    @tailrec
    def loopSolve(squares: List[List[(Int, Int)]], safeCombination: Set[Chess]): Set[Chess] = {
      if (squares.isEmpty) safeCombination
      else {
        val square = squares.head.map(xy => Square(xy._1, xy._2))
        val newSafeCombination = safeCombination ++ loopTestPieces(square, all_pieces, safeCombination)
        loopSolve(squares.drop(1), newSafeCombination)
      }
    }

    val squares = (for (x <- 1 to board.x; y <- 1 to board.y)
    yield (x, y)).toSet.subsets(pieces.size).map(_.toList).toList

    loopSolve(squares, Set())
  }

  def main(args: Array[String]): Unit = {
    println("Input: 3×3 board containing 2 Kings and 1 Rook")
    val solution = solve(Chess.Board(3, 3), List(Chess.King, Chess.King, Chess.Rock))
    println(f"I have ${solution.size} solutions. No more of 3 of them follow: ")
    solution.take(3).map(println)
  }

}
