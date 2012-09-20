package game.client

import game.server.GameServer
import util.Random

class SergeyLazarevPlayer(val name: String) extends Player {

  def makeTurn(boardList: List[List[List[Int]]]): (Int, Int) = {
    val board = boardList.map(_.map(_.toArray).toArray).toArray

    // check current player winning possibility
    val (x1, y1) = findWinningMove(board)
    if (x1 != -1) {
      return (x1, y1)
    }

    // check enemy player winning possibility
    changePlayer(board)
    val (x2, y2) = findWinningMove(board)
    if (x2 != -1) {
      return (x2, y2)
    }

    // check fork for current player
    changePlayer(board);
    val (x3, y3) = findFork(board)
    if (x3 != -1) {
      moveTo(board, x3, y3);
      changePlayer(board);
      if (countWinningMoves(board) == 0) {
        return (x3, y3)
      }
      changePlayer(board);
      removeFrom(board, x3, y3);
    }

    // check fork for enemy player
    changePlayer(board);
    val (x4, y4) = findFork(board)
    if (x4 != -1) {
      moveTo(board, x4, y4);
      if (countWinningMoves(board) == 0) {
        return (x4, y4)
      }
      removeFrom(board, x4, y4);
    }


    // finding best move
    var max = 0;
    var point = (0, 0);
    for (i <- 0 to 3; j <- 0 to 3) {
      if (notFull(board, i, j)) {
        point = (i, j);
        max = getMoveCost(board, i, j);
      }
    }

    for (i <- 0 to 3; j <- 0 to 3) {
      if (notFull(board, i, j)) {
        val v = getMoveCost(board, i, j);
        if (v > max) {
          max = v;
          point = (i, j);
        }
      }
    }

    return point;
  }

  private def moveTo(board: Array[Array[Array[Int]]], x: Int, y: Int) {
    val z = board(x)(y).indexOf(0);
    board(x)(y)(z) = 1;
  }

  private def removeFrom(board: Array[Array[Array[Int]]], x: Int, y: Int) {
    var z = 3;
    if (notFull(board, x, y)) {
      z = board(x)(y).indexOf(0) - 1;
    }
    board(x)(y)(z) = 0;
  }

  private def getMoveCost(board: Array[Array[Array[Int]]], x: Int, y: Int): Int = {
    val z = board(x)(y).indexOf(0);
    var res = 0;

    for (dx <- -1 to 1; dy <- -1 to 1; dz <- -1 to 1) {
      if (dx != 0 || dy != 0 || dz != 0) {
        var pointsOnLine = 0;
        var my = 0;
        var enemy = 0;

        for (d <- -3 to 3) {
          val nx = x + d * dx;
          val ny = y + d * dy;
          val nz = z + d * dz;

          if (0.to(3).contains(nx) && 0.to(3).contains(ny) && 0.to(3).contains(nz)) {
            pointsOnLine += 1;
            if (board(nx)(ny)(nz) == 1) {
              my += 1;
            } else if (board(nx)(ny)(nz) == -1) {
              enemy += 1;
            }
          }
        }

        if (pointsOnLine == 4 && (my == 0 || enemy == 0) && (my != 0 || enemy != 0)) {
          res += my + enemy;
        }
      }
    }

    return res;
  }

  private def findFork(board: Array[Array[Array[Int]]]): (Int, Int) = {
    for (i <- 0 to 3; j <- 0 to 3) {
      if (notFull(board, i, j)) {
        moveTo(board, i, j);
        val winningMoves = countWinningMoves(board);
        removeFrom(board, i, j);
        if (winningMoves > 1) {
          return (i, j)
        }
      }
    }
    return (-1, -1)
  }


  private def countWinningMoves(board: Array[Array[Array[Int]]]): Int = {
    var result = 0
    for (i <- 0 to 3; j <- 0 to 3) {
      if (notFull(board, i, j) && isWinningMove(board, i, j)) {
        result = result + 1
      }
    }
    return result
  }

  private def findWinningMove(board: Array[Array[Array[Int]]]): (Int, Int) = {
    for (i <- 0 to 3; j <- 0 to 3) {
      if (notFull(board, i, j) && isWinningMove(board, i, j)) {
        return (i, j)
      }
    }
    (-1, -1)
  }

  private def isWinningMove(board: Array[Array[Array[Int]]], x: Int, y: Int): Boolean = {
    val k = board(x)(y).indexOf(0)
    board(x)(y)(k) = 1
    val result = GameServer.checkWinningCondition(convertToList(board)) == 1
    board(x)(y)(k) = 0
    return result
  }

  private def changePlayer(board: Array[Array[Array[Int]]]) {
    for (i <- 0 to 3; j <- 0 to 3; k <- 0 to 3) {
      board(i)(j)(k) = board(i)(j)(k) * -1
    }
  }

  private def notFull(board: Array[Array[Array[Int]]], x: Int, y: Int) = board(x)(y).contains(0)

  private def convertToList(board: Array[Array[Array[Int]]]): List[List[List[Int]]] = board.map(_.map(_.toList).toList).toList

}