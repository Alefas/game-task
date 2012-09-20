package game.client

import collection.mutable.ArrayBuffer
import collection.mutable
import java.util.concurrent.{TimeoutException, TimeUnit, Callable, Executors}

class IvanovakAlpha2(val name: String) extends Player {
  import IvanovakAlpha2._
  type TripArray = Array[Array[Array[Int]]]
  type TripList = List[List[List[Int]]]
  type Coord = (Int, Int, Int)

  private def isWinningMove(x: Int, y: Int, board: TripArray) : Boolean =
    checkFour(x, y, BLACK, board)

  private def isForcedMove(x: Int, y: Int, board: TripArray) : Boolean =
    checkFour(x, y, WHITE, board)

  private def avoidMove(x: Int, y: Int, board: TripArray): Boolean = {
    val z = board(x)(y).indexOf(EMPTY)

    def rollback() = board(x)(y)(z) = EMPTY

    board(x)(y)(z) = BLACK

    try {
      for (i <- 0 to 3; j <- 0 to 3) {
        if (board(i)(j).contains(EMPTY) && checkFour(i, j, WHITE, board))
          return true;
      }

      return false;
    } finally {
      rollback();
    }
  }

  private def isAForkMove(x: Int, y: Int, board: TripArray, color: Int): Boolean = {
    val z = board(x)(y).indexOf(EMPTY)
    board(x)(y)(z) = color

    var cnt = 0
    for (i <- 0 to 3; j <- 0 to 3) {
      if (board(i)(j).contains(EMPTY) && checkFour(i, j, color, board))
        cnt += 1
    }

    board(x)(y)(z) = EMPTY
    cnt >= 2
  }

  /**
   * The contract to the method is that there is no 1-turn win moves, and forks available.
   * Opponent haven't got 1-turn win move also, but might have fork move.
   * @param x x-coordinate of a candidate forcing move
   * @param y y-coordinate of a candidate forcing move
   * @param board playboard
   */
  private def isAChainOfForcingMoves(x: Int, y: Int, board: TripArray,
                                     color: Int): Boolean = {
    val bz = board(x)(y).indexOf(EMPTY)
    def rollback() = board(x)(y)(bz) = EMPTY

    board(x)(y)(bz) = color

    def isAChainOfForcingMovesHelper() : Boolean = {
      for (i <- 0 to 3; j <- 0 to 3)
        if (board(i)(j).contains(EMPTY) && checkFour(i, j, color, board)) {
          val wz = board(i)(j).indexOf(EMPTY)
          def rollbackInner() = board(i)(j)(wz) = EMPTY

          board(i)(j)(wz) = -1 * color

          try {
            for (ii <- 0 to 3; jj <- 0 to 3)
              if (board(ii)(jj).contains(EMPTY) && isAForkMove(ii, jj, board, color))
                return true

            for (ii <- 0 to 3; jj <- 0 to 3)
              if (board(ii)(jj).contains(EMPTY) && isAChainOfForcingMoves(ii, jj, board, color))
                return true
          } finally {
            rollbackInner()
          }
        }
      return false
    }

    try {
      return isAChainOfForcingMovesHelper()
    } finally {
      rollback();
    }
  }

  private def deniesForkMove(x: Int, y: Int, board: TripArray): Boolean = {
    val z = board(x)(y).indexOf(EMPTY)
    board(x)(y)(z) = BLACK
    def rollback() = board(x)(y)(z) = EMPTY

    def deniesForkMoveHelper() : Boolean = {
      for (i <- 0 to 3; j <- 0 to 3) {
        if (board(i)(j).contains(EMPTY) &&
          isAForkMove(i, j, board, WHITE))
          return false
      }

      return true
    }

    try {
      return deniesForkMoveHelper()
    } finally {
      rollback()
    }
  }

  /**
   * Method that checks 4 in a row figures, after making a turn into (x, y)
   * The contract to the method is that there is no 4 in row figures on the board.
   *
   * @param x x-coordinate
   * @param y y-coordinate
   * @param checkValue color of the figure
   * @param board board
   * @return true if the 4 row figure appears
   */
  private def checkFour(x: Int, y: Int, checkValue: Int, board: TripArray): Boolean = {
    val z = board(x)(y).indexOf(EMPTY)
    board(x)(y)(z) = checkValue

    def rollback() = board(x)(y)(z) = EMPTY

    def check(f: Int => Int): Boolean = {
      for (j <- 0 to 3) {
        if (f(j) != checkValue)
          return false
      }
      true
    }

    def checkFourHelper() : Boolean = {
      for (i <- 0 to 3) {
        if (check(board(_)(y)(z))) return true;
        if (check(board(x)(_)(z))) return true;
        if (check(board(x)(y)(_))) return true;

        if (check(p => board(p)(i)(p))) return true;
        if (check(p => board(i)(p)(p))) return true;

        if (check(p => board(i)(p)(3 - p))) return true;
        if (check(p => board(p)(i)(3 - p))) return true;
        if (check(p => board(p)(3 - p)(i))) return true;
      }

      if (check(p => board(p)(p)(p))) return true;
      if (check(p => board(p)(p)(3 - p))) return true;
      if (check(p => board(p)(3 - p)(3 - p))) return true;
      if (check(p => board(p)(3 - p)(p))) return true;

      if (check(p => board(p)(p)(z))) return true;
      return false;
    }

    try {
      return checkFourHelper();
    } finally {
      rollback();
    }
  }

  /**
   * @param board 0 - EMPTY field, 1 - your field, -1 - opponent field
   * @return tuple with your correct turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board1: TripList): (Int, Int) = {
    def toArray: TripArray = board1.map(_.map(_.toArray).toArray).toArray
    val board = toArray

    var theWinningMove: Option[(Int, Int)] = None
    var theForcedMove: Option[(Int, Int)] = None
    var chainMove: Option[(Int, Int)] = None
    var forkMove: Option[(Int, Int)] = None

    val possibleMoves: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]()
    val imPossibleMoves: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]()

    def Helper() : (Int, Int) = {
      for (i <- 0 to 3; j <- 0 to 3) {
        if (board(i)(j).contains(EMPTY))
          possibleMoves += ((i, j))
      }

      // 1 move depth
      possibleMoves.foreach {
        case (i, j) => {
          if (isWinningMove(i, j, board)) {
            theWinningMove = Some((i, j))
          }
          if (isForcedMove(i, j, board)) {
            theForcedMove = Some((i, j))
          }
        }
      }

      if (theWinningMove != None) {
        return theWinningMove.get
      } else if (theForcedMove != None) {
        return theForcedMove.get
      }

      // There are a few moves that can make opponent win in 1 turn
      // we must avoid them
      for (i <- 0 to 3; j <- 0 to 3) {
        if (board(i)(j).contains(EMPTY) && avoidMove(i, j, board)) {
          possibleMoves -= ((i, j))
        }
      }

      possibleMoves.foreach {
        case (i, j) => {
          // attempt to end the game in 2 moves, and using a forcing strategy in n moves
          if (isAForkMove(i, j, board, BLACK)) {
            forkMove = Some((i, j))
          }
          if (isAChainOfForcingMoves(i, j, board, BLACK)) {
            chainMove = Some((i, j))
          }
        }
      }

      if (forkMove != None) {
        // here we take 0 element, because its no matter for us how to win
        return forkMove.get
      } else if (chainMove != None) {
        // here we take 0 element, because its no matter for us how to win
        return chainMove.get
      }

      // if we can't find an attack, lets search for the opponent's attack
      possibleMoves.foreach {
        case (i, j) => {
          if (!deniesForkMove(i, j, board)) {
            imPossibleMoves += ((i, j))
          }
        }
      }

      possibleMoves --= imPossibleMoves
      makeGreedyMove(board1, possibleMoves.toList)
    }

    val executor = Executors.newSingleThreadExecutor();
    val future = executor.submit(new Callable[(Int, Int)] {
      def call() = Helper()
    })
    val (x, y) =
      try {
        future.get(TIMEOUT, TimeUnit.MILLISECONDS)
      } catch {
        case e: TimeoutException => {
          return makeGreedyMove(board1, possibleMoves.toList)
        } //forfeiture
      }
    executor.shutdownNow()
    return (x, y)
    // finally if there is nothing we've found, lets make just a gridy move.

  }

  private def printBoard(board: TripArray) {
    board.foreach((aa) => {
      aa.foreach(t => print(s"${t(0)} "))
      println()
    })
  }

  private def makeGreedyMove(board: TripList, possibleMoves: List[(Int, Int)]): (Int, Int) = {
    val vecsEnemy: mutable.Set[List[Coord]] = mutable.Set.empty
    val vecsOurs: mutable.Set[List[Coord]] = mutable.Set.empty

    addVecsToSet(vecsEnemy)
    addVecsToSet(vecsOurs)

    for (i <- 0 to 3; j <- 0 to 3) {
      val z = board(i)(j).indexOf(EMPTY)

      if (z != -1) {
        for (k <- 0 until z) {
          if (board(i)(j)(k) == -1)
            vecsOurs.filter((ls: List[Coord]) => ls.contains((i, j, k)))
              .foreach((ls: List[Coord]) => vecsOurs.remove(ls))
          else
            vecsEnemy.filter((ls: List[Coord]) => ls.contains((i, j, k)))
              .foreach((ls: List[Coord]) => vecsEnemy.remove(ls))

        }
      }
    }

    var maxOurs = 0
    var maxBlocked = 0
    var res = (0, 0)
    // Searching for the `greediest move`
    possibleMoves.foreach {
      case (i, j) => {
        val z = board(i)(j).indexOf(EMPTY)
        val sizeBlocked = vecsEnemy.filter((ls: List[Coord]) => ls.contains((i, j, z))).size
        val sizeOurs = vecsOurs.filter((ls: List[Coord]) => ls.contains((i, j, z))).size

        if (sizeBlocked > maxBlocked) {
          maxBlocked = sizeBlocked
          maxOurs = sizeOurs
          res = (i, j)
        } else if (sizeBlocked == maxBlocked && sizeOurs > maxOurs) {
          maxBlocked = sizeBlocked
          maxOurs = sizeOurs
          res = (i, j)
        }
      }
    }

    res
  }

  private def addVecsToSet(set: mutable.Set[List[Coord]]): Unit = {
    for (i <- 0 to 3; j <- 0 to 3) {
      set += List((i, j, 0), (i, j, 1), (i, j, 2), (i, j, 3))
      set += List((i, 0, j), (i, 1, j), (i, 2, j), (i, 3, j))
      set += List((0, i, j), (1, i, j), (2, i, j), (3, i, j))
    }

    for (i <- 0 to 3) {
      set += List((0, 0, i), (1, 1, i), (2, 2, i), (3, 3, i))
      set += List((0, i, 0), (1, i, 1), (2, i, 2), (3, i, 3))
      set += List((i, 0, 0), (i, 1, 1), (i, 2, 2), (i, 3, 3))

      set += List((0, 3, i), (1, 2, i), (2, 1, i), (3, 0, i))
      set += List((0, i, 3), (1, i, 2), (2, i, 1), (3, i, 0))
      set += List((i, 0, 3), (i, 1, 2), (i, 2, 1), (i, 3, 0))
    }

    set += List((0, 0, 3), (1, 1, 2), (2, 2, 1), (3, 3, 0))
    set += List((0, 0, 0), (1, 1, 1), (2, 2, 2), (3, 3, 3))
    set += List((3, 0, 0), (2, 1, 1), (1, 2, 2), (0, 3, 3))
    set += List((3, 0, 3), (2, 1, 2), (1, 2, 1), (0, 3, 0))

  }
}

private object IvanovakAlpha2 {
  val BLACK = 1
  val WHITE = -1
  val EMPTY = 0
  val TIMEOUT = 900
}