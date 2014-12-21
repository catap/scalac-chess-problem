import org.scalatest.FreeSpec

class ChessProblem$Test extends FreeSpec {
  "Check solution" - {
    "3×3 board containing 2 Kings and 1 Rook" - {
      val solution = ChessProblem.solve(Chess.Board(2, 2), List(Chess.King, Chess.King, Chess.Rock))

      assert(solution.size == 4)
      assert(solution.mkString("\n") ==
          "♔**\n" +
          "**♖\n" +
          "♔**\n" +
          "\n" +
          "♔*♔\n" +
          "***\n" +
          "*♖*\n" +
          "\n" +
          "*♖*\n" +
          "***\n" +
          "♔*♔\n" +
          "\n" +
          "**♔\n" +
          "♖**\n" +
          "**♔\n")
    }
  }
}
