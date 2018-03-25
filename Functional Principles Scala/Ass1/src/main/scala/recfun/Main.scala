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
      if (c<0 || c>r || r<0) {
        0
      }
      else {
        if (c==0 || r==c)
          1
        else
          pascal(c-1, r-1)+pascal(c,r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def countyet(bal: Int, leftStr: List[Char]): Boolean = {
        if (leftStr.length == 0) (bal == 0)
        else if (bal < 0) false
        else if (leftStr.head == '(') countyet(bal+1, leftStr.tail)
        else if (leftStr.head == ')') countyet(bal-1, leftStr.tail)
        else countyet(bal, leftStr.tail)
      }
      countyet(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money==0)
        1
      else if(coins.isEmpty || money < 0)
        0
      else
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
