package game.client

import util.Random

/**
 * @author Alefas
 * @since 25.08.12
 */

class RandomPlayer(val name: String) extends Player {
  /**
   * @param board 0 - empty field, 1 - your field, 2 - opponent field
   * @return tuple with your turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: List[List[List[Int]]]): (Int, Int) = {
    while (true) {
      val (i, j) = (Random.nextInt(4), Random.nextInt(4))

      if (board(i)(j).contains(0)) return (i, j)
    }
    (0, 0)
  }
}
