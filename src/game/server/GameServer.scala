package game.server

import game.client._
import collection.mutable
import java.util.concurrent.{TimeUnit, TimeoutException, Callable, Executors}

/**
 * @author Alefas
 * @since 25.08.12
 */

object GameServer {
  val TIMEOUT = 1000

  private val players: List[Player] =
    List(
      new RandomPlayer("Random"),
      new StackPlayer("Stack"),
      new SergeyLazarevPlayer("Sergey Lazarev"),
      new BashorovSmartPlayer("Zalim Basharov"),
      new AlinaPevznerPlayer("Alina Pevzner"),
      new IvanovakAlpha2("Anton Ivanov"),
      new CriticalPositivePlayer("Alexey Velikiy")
    )

  def main(args: Array[String]) {
    val results = mutable.HashMap.empty[String, Int]
    for (player <- players) results += ((player.name, 0))
    for (first <- players; second <- players if first ne second) {
      val gameResult = game(first, second)
      if (gameResult == 0) {
        results.update(first.name, results(first.name) + 1)
        results.update(second.name, results(second.name) + 1)
      }
      if (gameResult == 1) {
        results.update(first.name, results(first.name) + 2)
      }
      if (gameResult == -1) {
        results.update(second.name, results(second.name) + 2)
      }
    }
    results.toList.sortBy(-_._2).foreach {
      case tuple =>
        val (player, score) = tuple
        println(s"${player}: ${0.5 * score}")
    }
    System.exit(0)
  }

  private def game(first: Player, second: Player, board: Array[Array[Array[Int]]] = Array.fill(4, 4, 4)(0)): Int = {
    def convertToList: List[List[List[Int]]] = board.map(_.map(_.toList).toList).toList

    //make turn
    val listBoard = convertToList
    val executor = Executors.newSingleThreadExecutor();
    val future = executor.submit(new Callable[(Int, Int)] {
      def call() = first.makeTurn(listBoard)
    })
    val (x, y) =
      try {
        future.get(TIMEOUT, TimeUnit.MILLISECONDS)
      }
      catch {
        case e: TimeoutException => return -1 //forfeiture
      }
    executor.shutdownNow()
    if (!0.to(3).contains(x) || !0.to(3).contains(y)) return -1 //forfeiture
    //update board
    val index = board(x)(y).indexOf(0)
    if (index == -1) return -1 //forfeiture
    board(x)(y)(index) = 1
    //check board
    val winningCondition = checkWinningCondition(convertToList)
    if (winningCondition >= 0) return winningCondition
    //update board
    for (i <- 0 to 3; j <- 0 to 3; k <- 0 to 3) board(i)(j)(k) = board(i)(j)(k) * -1
    -game(second, first, board)
  }

  /**
   * This method doesn't check player '-1' winning condition
   * @param board game board
   * @return 1 - player with '1' wins, 0 - it's a tie, -1 - no winners or player '-1' wins
   */
  def checkWinningCondition(board: List[List[List[Int]]]): Int = {
    def check(fun: Int => Int): Boolean = {
      for (k <- 0 to 3) {
        if (fun(k) != 1) return false
      }
      true
    }
    for (i <- 0 to 3; j <- 0 to 3) {
      if (check(board(i)(j)(_))) return 1
      if (check(board(i)(_)(j))) return 1
      if (check(board(_)(i)(j))) return 1
    }
    for (i <- 0 to 3) {
      if (check(p => board(i)(p)(p))) return 1
      if (check(p => board(p)(i)(p))) return 1
      if (check(p => board(p)(p)(i))) return 1
      if (check(p => board(i)(p)(3 - p))) return 1
      if (check(p => board(p)(i)(3 - p))) return 1
      if (check(p => board(p)(3 - p)(i))) return 1
    }
    if (check(p => board(p)(p)(p))) return 1
    if (check(p => board(p)(p)(3 - p))) return 1
    if (check(p => board(p)(3 - p)(3 - p))) return 1
    if (check(p => board(p)(3 - p)(p))) return 1
    if (board.forall(_.forall(_.forall(_ != 0)))) return 0
    return -1
  }
}
