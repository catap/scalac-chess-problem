import Chess._
import org.scalatest.FreeSpec

class Chess$Test extends FreeSpec {
  "Move pieces on board" - {
    "Board 1x1" - {
      val board = Board(1, 1)
      val square = Square(1, 1)

      "King shouldn't have any possible position" - {
        assert(King.possibleSquares(square, board).size == 0)
      }

      "Queen shouldn't have any possible position" - {
        assert(Queen.possibleSquares(square, board).size == 0)
      }

      "Rock shouldn't have any possible position" - {
        assert(Rock.possibleSquares(square, board).size == 0)
      }

      "Bishop shouldn't have any possible position" - {
        assert(Bishop.possibleSquares(square, board).size == 0)
      }

      "Knight shouldn't have any possible position" - {
        assert(Knight.possibleSquares(square, board).size == 0)
      }
    }

    "Board 2x2" - {
      val board = Board(2, 2)
      val square = Square(1, 1)

      "King should have 3 possible position" - {
        assert(King.possibleSquares(square, board).size == 3)
      }

      "Queen should have 3 possible position" - {
        assert(Queen.possibleSquares(square, board).size == 3)
      }

      "Rock should have 2 possible position" - {
        assert(Rock.possibleSquares(square, board).size == 2)
      }

      "Bishop should have 1 possible position" - {
        assert(Bishop.possibleSquares(square, board).size == 1)
      }

      "Knight shouldn't have any possible position" - {
        assert(Knight.possibleSquares(square, board).size == 0)
      }
    }

    "Board 8x8" - {
      val board = Board(8, 8)
      val center = Square(5, 4)
      val corner = Square(1, 1)
      val wall = Square(5, 8)

      "King should have " - {
        "8 possible position at center" - {
          assert(King.possibleSquares(center, board).size == 8)
        }
        "3 possible position at corner" - {
          assert(King.possibleSquares(corner, board).size == 3)
        }
        "5 possible position at wall" - {
          assert(King.possibleSquares(wall, board).size == 5)
        }
      }

      "Queen should have " - {
        "27 possible position at center (5, 4)" - {
          assert(Queen.possibleSquares(center, board).size == 27)
        }
        "21 possible position at corner" - {
          assert(Queen.possibleSquares(corner, board).size == 21)
        }
        "21 possible position at wall" - {
          assert(Queen.possibleSquares(wall, board).size == 21)
        }
      }

      "Rock should have " - {
        "14 possible position at center" - {
          assert(Rock.possibleSquares(center, board).size == 14)
        }
        "14 possible position at corner" - {
          assert(Rock.possibleSquares(corner, board).size == 14)
        }
        "14 possible position at wall" - {
          assert(Rock.possibleSquares(wall, board).size == 14)
        }
      }

      "Bishop should have " - {
        "13 possible position at center (5, 4)" - {
          assert(Bishop.possibleSquares(center, board).size == 13)
        }
        "7 possible position at corner" - {
          assert(Bishop.possibleSquares(corner, board).size == 7)
        }
        "7 possible position at wall" - {
          assert(Bishop.possibleSquares(wall, board).size == 7)
        }
      }

      "Knight should have " - {
        "8 possible position at center" - {
          assert(Knight.possibleSquares(center, board).size == 8)
        }
        "2 possible position at corner" - {
          assert(Knight.possibleSquares(corner, board).size == 2)
        }
        "4 possible position at wall" - {
          assert(Knight.possibleSquares(wall, board).size == 4)
        }
      }
    }
  }

  "Threaten on" - {
    "board 1x1" - {
      val board = Board(1, 1)
      val square = Square(1, 1)
      "Nobody shouldn't threaten to him-self" - {
        assert(!King.isThreatens(square, board, List(square)))
        assert(!Chess(board).addPiece(King, square).isThreaten)
      }
    }

    "board 2x2" - {
      val board = Board(2, 2)
      val square1 = Square(1, 1)
      val square2 = Square(2, 2)

      "King should threaten to another piece" - {
        assert(King.isThreatens(square1, board, List(square2)))
        assert(Chess(board)
          .addPiece(King, square1)
          .addPiece(King, square2)
          .isThreaten)
      }
    }

    "board 8x8" - {
      val board = Board(8, 8)
      val center1 = Square(5, 4)
      val center2 = Square(4, 5)
      val corner1 = Square(1, 1)
      val corner2 = Square(8, 8)

      "King at center should threaten to another King at center" - {
        assert(King.isThreatens(center1, board, List(center2)))
        assert(Chess(board)
          .addPiece(King, center1)
          .addPiece(King, center2)
          .isThreaten)
      }

      "King at center shouldn't threaten to another King at corner" - {
        assert(!King.isThreatens(center1, board, List(corner1)))
        assert(!King.isThreatens(center1, board, List(corner2)))
        assert(!King.isThreatens(center1, board, List(corner1, corner2)))
        assert(!Chess(board)
          .addPiece(King, corner1)
          .addPiece(King, corner2)
          .isThreaten)
      }

      "King at center should threaten to someone on this board" - {
        assert(King.isThreatens(center1, board, List(center1, center2, corner1, corner2)))
        assert(Chess(board)
          .addPiece(King, corner1)
          .addPiece(King, corner2)
          .addPiece(King, center1)
          .addPiece(King, center2)
          .isThreaten)
      }
    }
  }

  "Dump to string chess" - {
    val board = Board(8, 8)
    val center = Square(5, 4)
    val corner = Square(1, 1)
    val wall = Square(5, 8)

    assert(Chess(board)
      .addPiece(King, center)
      .addPiece(King, corner)
      .addPiece(King, wall).toString ==
        "♔*------\n" +
        "**------\n" +
        "---***--\n" +
        "---*♔*--\n" +
        "---***--\n" +
        "--------\n" +
        "---***--\n" +
        "---*♔*--\n"
    )
  }
}
