import Chess._

class Chess$Test extends org.scalatest.FreeSpec {
  "Move pieces on board" - {
    "Board 1x1" - {
      val board = Board(0, 0)
      val position = Position(0, 0)

      "Pieces should don't have any possible position" - {
        assert(King.possiblePositions(position, board).size == 0)
      }
    }

    "Board 2x2" - {
      val board = Board(1, 1)
      val position = Position(0, 0)

      "King should have 3 possible position" - {
        assert(King.possiblePositions(position, board).size == 3)
      }
    }

    "Board 8x8" - {
      val board = Board(7, 7)
      val center = Position(4, 3)
      val corner = Position(0, 0)
      val wall = Position(4, 7)

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
  }
