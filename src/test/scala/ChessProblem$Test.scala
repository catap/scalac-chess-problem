import org.scalatest.FreeSpec

class ChessProblem$Test extends FreeSpec {
  "Check solution" - {
    "3×3 board containing 2 Kings and 1 Rook" - {
      val solution = ChessProblem.solve(Chess.Board(2, 2), List(Chess.King, Chess.King, Chess.Rock))

      assert(solution.size == 4)
      assert(solution.mkString("\n") ==
          "K**\n" +
          "**R\n" +
          "K**\n" +
          "\n" +
          "K*K\n" +
          "***\n" +
          "*R*\n" +
          "\n" +
          "*R*\n" +
          "***\n" +
          "K*K\n" +
          "\n" +
          "**K\n" +
          "R**\n" +
          "**K\n")
    }
  }
}
