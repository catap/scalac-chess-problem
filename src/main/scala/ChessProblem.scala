import Chess._

object ChessProblem {

  def solve(board: Board, pieces: List[Piece]) = {
    pieces.permutations.map ( pieces =>
      (for (x <- 0 to board.x; y <- 0 to board.y) yield (x,y))
        .permutations
        .map(xss => xss.take(pieces.size))
        .map(_.zip(pieces).map(p => Position(p._2, Square(p._1._1, p._1._2))) )
        .map(occupied => Chess(board, occupied.toSet))
        .filter(!_.isThreaten)
    ).flatten.toSet
  }

  def main(args: Array[String]): Unit = {
    println("Input: 3×3 board containing 2 Kings and 1 Rook")
    val solution = solve(Chess.Board(2, 2), List(Chess.King, Chess.King, Chess.Rock))
    println(f"I have ${solution.size} solutions. No more of 3 of them follow: ")
    solution.take(3).map(println)
  }

}
