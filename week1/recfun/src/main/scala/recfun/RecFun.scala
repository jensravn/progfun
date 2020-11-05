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
    if (c==0 || c-r==0) 1
    else pascal(c-1,r-1) + pascal(c,r-1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = balanceRec(0, chars)

  @tailrec
  def balanceRec(level:Int, chars: List[Char]): Boolean ={
    if(chars.isEmpty) level == 0
    else if(level < 0)  false
    else if(chars.head == '(') balanceRec(level+1, chars.tail)
    else if(chars.head == ')') balanceRec(level-1, chars.tail)
    else balanceRec(level, chars.tail)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
