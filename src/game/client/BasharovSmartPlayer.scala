package game.client

import Common.listBoard_t

/**
 * @author Zalim Bashorv
 */

class BashorovSmartPlayer(val name: String) extends Player {

  /**
   * @param board 0 - empty field, 1 - your field, -1 - opponent field
   * @return tuple with your correct turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: listBoard_t): (Int, Int) = {
    findPosition(board)
  }

  private class Rating(val mine3: Int, val opponent3: Int, val mine2free2: Int, val opponent2free2: Int, val mine1free3: Int, val free4: Int) {
    def +(a: Rating): Rating =
      new Rating(mine3 + a.mine3, opponent3 + a.opponent3, mine2free2 + a.mine2free2,
        opponent2free2 + a.opponent2free2, mine1free3 + a.mine1free3, free4 + a.free4)

    def <(a: Rating): Boolean = {
      if (mine3 != a.mine3)
        return mine3 < a.mine3

      if (opponent3 != a.opponent3)
        return opponent3 < a.opponent3

      if (mine2free2 != a.mine2free2)
        return mine2free2 != a.mine2free2

      if (opponent2free2 != a.opponent2free2)
        return opponent2free2 < a.opponent2free2

      if (mine1free3 != a.mine1free3)
        return mine1free3 < a.mine1free3

      return free4 < a.free4
    }

    override def toString() = s"R($mine3, $opponent3, $mine2free2, $opponent2free2, $mine1free3, $free4)"
  }
  private object Rating {
    val zero = new Rating(0, 0, 0, 0, 0, 0)
    val mine3 = new Rating(1, 0, 0, 0, 0, 0)
    val opponent3 = new Rating(0, 1, 0, 0, 0, 0)
    val mine2free2 = new Rating(0, 0, 0, 1, 0, 0)
    val opponent2free2 = new Rating(0, 0, 0, 1, 0, 0)
    val mine1free3 = new Rating(0, 0, 0, 0, 1, 0)
    val free4 = new Rating(0, 0, 0, 0, 0, 1)
    val bad = new Rating(-1, -1, -1, -1, -1, -1)
  }

  private def findPosition(board: listBoard_t): (Int, Int) = {

    val rating = Array.fill(4, 4)(Rating.zero)

    def initMaxRating(): (Rating, (Int, Int)) = {
      for (i <- 0 to 3; j <- 0 to 3) {
        val k = board(i)(j).indexOf(0);
        if (k != -1) {
          return (calcRating(board, (i, j, k)), (i, j));
        }
      }
      (calcRating(board, (0, 0, 0)), (0, 0))
    }

    var (maxRating, maxRatingPos)= initMaxRating()

    for (i <- 0 to 3; j <- 0 to 3) {
      val k = board(i)(j).indexOf(0);
      if (k != -1) {
        val rating = calcRating(board, (i, j, k))
        if (maxRating < rating) {
          maxRating = rating
          maxRatingPos = (i, j)
        }
      }
    }

    maxRatingPos
  }

  private def calcRating(board: listBoard_t, newPos: (Int, Int, Int)): Rating = {
    val (x, y, k) =  newPos
    if (!(0 to 3).contains(k))
      return Rating.zero

    def check(fun: Int => Int): Rating = {
      var (mine, anothers, free) = (0, 0, 0)
      for (l <- 0 to 3) {
        val v = fun(l)
        if (v == 1)
          mine += 1
        else if (v == 0)
          free += 1
        else
          anothers += 1
      }

      if (free == 1) {
        if (mine == 3)
          return Rating.mine3
        else if (anothers == 3)
          return Rating.opponent3
      }

      if (k < 3 && calcRating(board, (x, y, k+1)).opponent3 > 0 ) {
        return Rating.bad
      }

      if (free == 2) {
        if (mine == 2 )
          return Rating.mine2free2
        else if (anothers == 2)
          return Rating.opponent2free2
      }

      if (free == 3 && mine == 1) {
        return Rating.mine1free3
      }

      if (free == 4) {
        return Rating.free4
      }

      Rating.zero
    }

    check(board(_)(y)(k)) +
      check(board(x)(_)(k)) +
      check(board(x)(y)(_)) +
      (if (x == y && y == k) check(p => board(p)(p)(p)) else Rating.zero) +
      (if (x == y && y == 3 - k) check(p => board(p)(p)(3 - p)) else Rating.zero) +
      (if (x == k && x == 3 - y) check(p => board(p)(3 - p)(p)) else Rating.zero) +
      (if (y == k && x == 3 - y) check(p => board(p)(3 - p)(3 - p)) else Rating.zero) +
      (if (x == y) check(p => board(p)(p)(k)) else Rating.zero) +
      (if (x == 3 - y) check(p => board(p)(3 - p)(k)) else Rating.zero) +
      (if (x == k) check(p => board(p)(y)(p)) else Rating.zero) +
      (if (x == 3 - k) check(p => board(p)(y)(3 - p)) else Rating.zero) +
      (if (y == k) check(p => board(x)(p)(p)) else Rating.zero) +
      (if (y == 3 - k) check(p => board(x)(p)(3 - p)) else Rating.zero)
  }
}

object Common {
  type listBoard_t = List[List[List[Int]]]
  type arrayBoard_t = Array[Array[Array[Int]]]

  /**
   * @param board game board
   * @param newPos checking position
   * @param player plyer identefier
   * @return 1 - player wins, 0 - it's a tie, -1 - no winners or opponent player wins
   */
  def checkWinningCondition(board: listBoard_t, newPos: (Int, Int), player: Int): Int = {
    def check(fun: Int => Int, fix: Int): Boolean = {
      for (l <- 0 to 3 if l != fix ) {
        if (fun(l) != player) return false
      }
      true
    }

    val (x, y) =  newPos
    val k = board(x)(y).indexOf(0)
    if (k == -1)
      return -1 //todo

    if (check(board(_)(y)(k), x)) return 1
    if (check(board(x)(_)(k), y)) return 1
    if (check(board(x)(y)(_), k)) return 1

    if (x == y && y == k && check(p => board(p)(p)(p), x)) return 1
    if (x == y && y == 3 - k && check(p => board(p)(p)(3 - p), x)) return 1

    if (x == k && x == 3 - y && check(p => board(p)(3 - p)(p), x)) return 1
    if (y == k && x == 3 - y && check(p => board(p)(3 - p)(3 - p), x)) return 1

    if (x == y     && check(p => board(p)(p)(k), x)) return 1
    if (x == 3 - y && check(p => board(p)(3 - p)(k), x)) return 1

    if (x == k     && check(p => board(p)(y)(p), x)) return 1
    if (x == 3 - k && check(p => board(p)(y)(3 - p), x)) return 1

    if (y == k     && check(p => board(x)(p)(p), y)) return 1
    if (y == 3 - k && check(p => board(x)(p)(3 - p), y)) return 1

    if (board.forall(_.forall(_.forall(_ != 0)))) return 0

    return -1
  }

  def printBoard(board: listBoard_t) {
    for (k <- 0 to 3) {
      for (i <- 0 to 3) {
        for (j <- 0 to 3)
          print(s"\t${board(i)(j)(k)}")
        println()
      }
      println()
    }
  }

  def printBoard(board: arrayBoard_t) {
    for (k <- 0 to 3) {
      for (i <- 0 to 3) {
        for (j <- 0 to 3)
          print(s"\t${board(i)(j)(k)}")
        println()
      }
      println()
    }
  }
}
