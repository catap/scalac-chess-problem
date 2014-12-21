import Chess._

class Chess$Test extends org.scalatest.FreeSpec {
  "Move pieces on board" - {
    "Board 1x1" - {
      val board = Board(0, 0)
      val position = Square(0, 0)

      "Pieces should don't have any possible position" - {
        assert(King.possiblePositions(position, board).size == 0)
      }
    }

    "Board 2x2" - {
      val board = Board(1, 1)
      val position = Square(0, 0)

      "King should have 3 possible position" - {
        assert(King.possiblePositions(position, board).size == 3)
      }
    }

    "Board 8x8" - {
      val board = Board(7, 7)
      val center = Square(4, 3)
      val corner = Square(0, 0)
      val wall = Square(4, 7)

      "King should have " - {
        "8 possible position at center" - {
          assert(King.possiblePositions(center, board).size == 8)
        }
        "3 possible position at corner" - {
          assert(King.possiblePositions(corner, board).size == 3)
        }
        "5 possible position at wall" - {
          assert(King.possiblePositions(wall, board).size == 5)
        }
      }
    }
  }

  "Threaten on" - {
    "board 1x1" - {
      val board = Board(0, 0)
      val position = Square(0, 0)
      "Nobody shouldn't threaten to him-self" - {
        assert(!King.isThreatens(position, board, Set(position)))
      }
    }

    "board 2x2" - {
      val board = Board(1, 1)
      val position1 = Square(0, 0)
      val position2 = Square(1, 1)

      "King should threaten to another piece" - {
        assert(King.isThreatens(position1, board, Set(position2)))
      }
    }

    "board 8x8" - {
      val board = Board(7, 7)
      val center1 = Square(4, 3)
      val center2 = Square(3, 4)
      val corner1 = Square(0, 0)
      val corner2 = Square(7, 7)

      "King at center should threaten to another King at center" - {
        assert(King.isThreatens(center1, board, Set(center2)))
      }

      "King at center shouldn't threaten to another King at corner" - {
        assert(!King.isThreatens(center1, board, Set(corner1)))
        assert(!King.isThreatens(center1, board, Set(corner2)))
        assert(!King.isThreatens(center1, board, Set(corner1, corner2)))
      }

      "King at center should threaten to someone on this board" - {
        assert(King.isThreatens(center1, board, Set(center1, center2, corner1, corner2)))
      }
    }
  }
}
