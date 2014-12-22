import org.scalatest.FreeSpec

class ChessProblem$Test extends FreeSpec {
  "Check solution" - {
    "3×3 board containing 2 Kings and 1 Rook" - {
      val solution = ChessProblem.solve(Chess.Board(3, 3), List(Chess.King, Chess.King, Chess.Rock))

      assert(solution.size == 4)
      assert(solution.mkString("\n") ==
        "♔*♔\n" +
          "***\n" +
          "*♖*\n" +
          "\n" +
          "**♔\n" +
          "♖**\n" +
          "**♔\n" +
          "\n" +
          "♔**\n" +
          "**♖\n" +
          "♔**\n" +
          "\n" +
          "*♖*\n" +
          "***\n" +
          "♔*♔\n")
    }

    "4×4 board containing 2 Rooks and 4 Knights" - {
      val solution = ChessProblem.solve(Chess.Board(4, 4), List(Chess.Knight, Chess.Knight, Chess.Knight, Chess.Knight, Chess.Rock, Chess.Rock))

      assert(solution.size == 8)
      assert(solution.mkString("\n") ==
          "**♖*\n" +
          "*♘*♘\n" +
          "♖***\n" +
          "*♘*♘\n" +
          "\n" +
          "♘*♘*\n" +
          "*♖**\n" +
          "♘*♘*\n" +
          "***♖\n" +
          "\n" +
          "*♘*♘\n" +
          "♖***\n" +
          "*♘*♘\n" +
          "**♖*\n" +
          "\n" +
          "*♘*♘\n" +
          "**♖*\n" +
          "*♘*♘\n" +
          "♖***\n" +
          "\n" +
          "***♖\n" +
          "♘*♘*\n" +
          "*♖**\n" +
          "♘*♘*\n" +
          "\n" +
          "*♖**\n" +
          "♘*♘*\n" +
          "***♖\n" +
          "♘*♘*\n" +
          "\n" +
          "♘*♘*\n" +
          "***♖\n" +
          "♘*♘*\n" +
          "*♖**\n" +
          "\n" +
          "♖***\n" +
          "*♘*♘\n" +
          "**♖*\n" +
          "*♘*♘\n")
    }
  }
}
