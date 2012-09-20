package game.client

import util.Random

class CriticalPositivePlayer(val name: String) extends Player {

  private var myBoard: List[List[List[Int]]] = null
  private val myGoodSubst = "1110"
  private val myBadSubst = "2220"

  /**
   * @param board 0 - empty field, 1 - your field, 2 - opponent field
   * @return tuple with your turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: List[List[List[Int]]]): (Int, Int) = {
    myBoard = board
    //println(myBoard.toString())
    val criticalResult = checkCriticalCases();

    if (criticalResult !=(-1, -1)) {
      return criticalResult
    }

    val verticalClosestResult = findClosestVertical()
    if (verticalClosestResult.length > 0) {
      var rand = Math.abs(Random.nextInt()) % verticalClosestResult.length
      val res = verticalClosestResult(rand)
      return res
    }

    for (i <- 0 to 3; j <- 0 to 3) {
      if (myBoard(i)(j).contains(0))
        return (i, j)
    }
    (0, 0)

  }

  def findClosestVertical(): List[(Int, Int)] = {
    var bestCount = -1
    var bestPosition = (-1, -1)

    for (i <- 0 to 3; j <- 0 to 3) {
      val seq = lstToSeq(myBoard(i)(j))
      var count = -1;
      if (seq == "0000") count = 0
      if (seq == "1000") count = 1
      if (seq == "1100") count = 2
      if (seq == "1110") count = 3
      if (bestCount < count) {
        bestCount = count
        bestPosition = (i, j)
      }
    }

    var possibleSolutions: List[(Int, Int)] = Nil
    if (bestCount == -1) return possibleSolutions

    for (i <- 0 to 3; j <- 0 to 3) {
      val seq = lstToSeq(myBoard(i)(j))
      var count = -1;
      if (seq == "0000") count = 0
      if (seq == "1000") count = 1
      if (seq == "1100") count = 2
      if (seq == "1110") count = 3
      if (bestCount == count) {
        possibleSolutions = (i, j) :: possibleSolutions
      }
    }

    return possibleSolutions
  }

  def checkCriticalCases(): (Int, Int) = {

    // check vertical
    for (i <- 0 to 3; j <- 0 to 3) {
      val seq = getVerticalStr(i, j)
      if (seq.indexOf(myBadSubst) != -1 || seq.indexOf(myGoodSubst) != -1)
        return (i, j)
    }

    //check rows
    for (i <- 0 to 3; z <- 0 to 3) {
      val seq = getHorizontalRowLineStr(i, z)
      var crI = resolveCriticalIndex(seq)
      if (crI != -1 && myBoard(i)(crI).indexOf(0) == z)
        return (i, crI)
    }

    //check cols
    for (i <- 0 to 3; z <- 0 to 3) {
      val seq = getHorizontalColumnLineStr(i, z)
      var crI = resolveCriticalIndex(seq)
      if (crI != -1 && myBoard(crI)(i).indexOf(0) == z)
        return (crI, i)
    }

    //check diagonal A
    for (z <- 0 to 3) {
      val seq = getDiagonalAStr(z)
      var crI = resolveCriticalIndex(seq)
      if (crI != -1 && myBoard(crI)(crI).indexOf(0) == z)
        return (crI, crI)
    }

    //check diagonal B
    for (z <- 0 to 3) {
      val seq = getDiagonalBStr(z)
      var crI = resolveCriticalIndex(seq)
      if (crI != -1 && myBoard(crI)(3 - crI).indexOf(0) == z)
        return (crI, 3 - crI)
    }

    (-1, -1)
  }

  def getVerticalStr(i: Int, j: Int) = lstToSeq(myBoard(i)(j))

  def getHorizontalRowLineStr(i: Int, z: Int) = lstToSeq(myBoard(i).map(i => i(z)))

  def getHorizontalColumnLineStr(i: Int, z: Int) =
    lstToSeq(myBoard(0)(i)(z) :: myBoard(1)(i)(z) ::
      myBoard(2)(i)(z) :: myBoard(3)(i)(z) :: Nil)

  def getDiagonalAStr(z: Int) =
    lstToSeq(myBoard(0)(0)(z) :: myBoard(1)(1)(z) ::
      myBoard(2)(2)(z) :: myBoard(3)(3)(z) :: Nil)

  def getDiagonalBStr(z: Int) =
    lstToSeq(myBoard(0)(3)(z) :: myBoard(1)(2)(z) ::
      myBoard(2)(3)(z) :: myBoard(3)(0)(z) :: Nil)

  def lstToSeq(lst: List[Int]) = lst.foldLeft("")((a, b) => a + (if (b == -1) 2 else b).toString())

  def resolveCriticalIndex(seq: String): Int = {
    if (seq == "0111" || seq == "0222") return 0
    if (seq == "1011" || seq == "2022") return 1
    if (seq == "1101" || seq == "2202") return 2
    if (seq == "1110" || seq == "2220") return 3
    return -1;
  }

}
