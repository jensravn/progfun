package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 30) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }


  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c - r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balanceRec(level: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) level == 0
      else if (level < 0) false
      else if (chars.head == '(') balanceRec(level + 1, chars.tail)
      else if (chars.head == ')') balanceRec(level - 1, chars.tail)
      else balanceRec(level, chars.tail)
    }

    balanceRec(0, chars)
  }

  // accumulator (acc)
  //     -
  //                  chars.head   chars.tail
  //                   _ x (head) :: y (tail)
  // chars = ((Hej))      (          (Hej))

  // balance(`((Hej))`) ->
  // return balanceRec(0,`((Hej))`) ->
  // return balanceRec(1,`(Hej))`) ->
  // balanceRec(2, `Hej))`) ->
  // balanceRec(2, `ej))`) ->
  // balanceRec(2, `j))`) ->
  // balanceRec(2, `))`) ->
  // balanceRec(1, `)`) ->
  // return balanceRec(0, ``)
  // return true
  // ------- if chars had one more `)`
  // return balanceRec(-1, ``)
  // return false

  // (
  // (
  // )
  // (


  /**
   * Exercise 2, another solution
   */
  def balance2(chars: List[Char]): Boolean = {
    def inner(c: List[Char], count: Int): Boolean = c match {
      case Nil => count == 0 // Line 1
      case ')' :: _ if count < 1 => false // Line 2
      case ')' :: xs => inner(xs, count - 1) // Line 3
      case '(' :: xs => inner(xs, count + 1) // Line 4
      case _ :: xs => inner(xs, count) // Line 5
    }

    inner(chars, 0)
  }

  /**
   * Exercise 3
   */
  // 2 1
  // 1 1 1
  // countChangeRec(0, 3, [1, 2])
  // countChangeRec(0, 1, [1, 2])
  // countChangeRec(0, 0, [1, 2])
  // +1
  // countChangeRec(0, 2, [1, 2])
  // countChangeRec(0, 0, [1, 2])
  // +1
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted(Ordering.Int.reverse);
    countChangeRec(0, money, sortedCoins)
  }

  def countChangeRec(combinations: Int, moneyRest: Int, coins: List[Int]): Int = {
    if (moneyRest == 0) { // New combination paid
      combinations + 1
    }
    else if (moneyRest < coins.last) { // Blind end (cannot pay)
      combinations
    }
    else if (moneyRest < coins.head) { // Greatest coin is too much
      countChangeRec(combinations, moneyRest, coins.tail)
    }
    else {
      // TODO: make immutable
      var mutableCombinations = combinations
      var mutableCoins = coins
      for (_ <- mutableCoins) {
        mutableCombinations += countChangeRec(combinations, moneyRest - mutableCoins.head, mutableCoins)
        mutableCoins = mutableCoins.tail
      }
      mutableCombinations
      // Recursive immutable implementation (instead of loop)
      // countChangeRec(combinations, moneyRest - coins.head, coins) + countChangeRec(combinations, moneyRest - coins.head, coins.tail)

      // TRY
//      val numbers = List(5, 4, 8, 6, 2)
//      numbers.fold(0) { (z, i) =>
//        a + i
//      }
      // result = 25

    }
  }
}
