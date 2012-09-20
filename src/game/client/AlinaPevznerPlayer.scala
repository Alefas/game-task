package game.client

/**
 * User: Alina Pevzner
 */
class AlinaPevznerPlayer(val name: String) extends Player {
  private var m_board = List(List(List(0)))

  private def findForTreePoint(): Option[(Int, Int)] = {
    for (i <- 0 to 3; j <- 0 to 3) {
      if (m_board(i)(j)(0) == 1 && m_board(i)(j)(1) == 1 && m_board(i)(j)(2) == 1 && m_board(i)(j)(3) == 0) {
        return Some((i, j))
      }
    }
    None
  }

  private def findForTwoPoint(): Option[(Int, Int)] = {
    for (i <- 0 to 3; j <- 0 to 3) {
      if (m_board(i)(j)(0) == 1 && m_board(i)(j)(1) == 1 && m_board(i)(j)(2) == 0) {
        return Some((i, j))
      }
    }
    None
  }

  private def findForOnePoint(): Option[(Int, Int)] = {
    for (i <- 0 to 3; j <- 0 to 3) {
      if (m_board(i)(j)(0) == 1 && m_board(i)(j)(1) == 0) {
        return Some((i, j))
      }
    }
    None
  }

  private def findAlternativePoint(): (Int, Int) = {
    for (i <- 0 to 3; j <- 0 to 3) {
      if (m_board(i)(j)(0) == 0) return (i, j)
    }
    for (i <- 0 to 3; j <- 0 to 3) {
      if (m_board(i)(j).contains(0)) return (i, j)
    }
    (0, 0)
  }

  /**
   * @param board 0 - empty field, 1 - your field, -1 - opponent field
   * @return tuple with your turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: List[List[List[Int]]]): (Int, Int) = {
    this.m_board = board
    findForTreePoint() orElse findForTwoPoint() orElse findForOnePoint() getOrElse findAlternativePoint()
  }

}