package game.client

class KrinkinPlayer(val name: String) extends Player {
  /**
   * @return expected estimation for (i,j,k) turn (the more the better)
   */
  private def estimate(board: List[List[List[Int]]], i: Int, j: Int, k: Int): Int = {
    def estimate_line(fun: Int => Int): Int = {
      def increment(acc: (Int, Int), index: Int): (Int, Int) = {
        fun(index) match {
          case 1  => (acc._1 + 1, acc._2)
          case -1 => (acc._1, acc._2 + 1)
          case _  => acc
        }
      }
      // counter._1 - mine, counter._2 - others
      val counter: (Int, Int) = (0 to 3).foldLeft((0, 0))(increment)

      //if mine and others in one line, we shoud no to attempt to capture
      //or to defend this line
      if ((counter._1 != 0) && (counter._2 != 0)) return 0

      //it is win turn
      if (counter._1 == 3) return 100000;

      //last attempt
      if (counter._2 == 3) return 1000;
    
      //defense is slightly preferable, otherwise
      counter._1*3 + counter._2*4 + 1
    }
    
    var estimation: Int = 0

    //simple rows and cols through (i,j,k)
    estimation += estimate_line(board(i)(j)(_))
    estimation += estimate_line(board(i)(_)(k))
    estimation += estimate_line(board(_)(j)(k))

    //bord diagonals
    if ((i == j) && (j == k)) estimation += estimate_line(x => board(x)(x)(x))
    if ((j == k) && (i + j == 3)) estimation += estimate_line(x => board(3 - x)(x)(x))
    if ((i == k) && (i + j == 3)) estimation += estimate_line(x => board(x)(3 - x)(x))
    if ((i == j) && (i + k == 3)) estimation += estimate_line(x => board(x)(x)(3 - x))

    //i-const slice diagonals
    if (j == k) estimation += estimate_line(x => board(i)(x)(x))
    if (j + k == 3) estimation += estimate_line(x => board(i)(x)(3 - x))

    //j-const slice diagonals
    if (i == k) estimation += estimate_line(x => board(x)(j)(x))
    if (i + k == 3) estimation += estimate_line(x => board(x)(j)(3 - x))

    //k-const slice diagonals
    if (i == j) estimation += estimate_line(x => board(x)(x)(k))
    if (i + j == 3) estimation += estimate_line(x => board(x)(3 - x)(k))

    estimation
  }

  /**
   * @param board 0 - empty field, 1 - your field, -1 - opponent field
   * @return tuple with your turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: List[List[List[Int]]]): (Int, Int) = {
    var best_turn: (Int, Int) = (0, 0)
    var best_estimation: Int = -1

    for (i <- 0 to 3; j <- 0 to 3) {
      val k = board(i)(j).indexOf(0)
      if (k != -1) {
        val expected: Int = estimate(board, i, j, k)
        if (expected > best_estimation) {
          best_turn = (i, j)
          best_estimation = expected
        }
      }
    }
    best_turn
  }
}
