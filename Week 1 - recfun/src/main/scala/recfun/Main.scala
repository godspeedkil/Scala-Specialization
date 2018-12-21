package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def newFunc(chars: List[Char], openPars: Int): Boolean = {
        if (chars.isEmpty) {
          openPars == 0
        } else {
          val head = chars.head
          val n =
            if (head == '(') openPars + 1
            else if (head == ')') openPars - 1
            else openPars
          if (n >= 0) newFunc(chars.tail, n)
          else false
        }
      }

      newFunc(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def newFunc(m: Int, c: List[Int]) : Int = {
        if (c.isEmpty) 0
        else if (m - c.head == 0) 1
        else if (m - c.head < 0) 0
        else countChange(m - c.head, c) + countChange(m, c.tail)
      }
      newFunc(money, coins.sorted)
    }
  }
