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
    def pascal(c: Int, r: Int): Int = 
      if ((c == 0) || (r == c)) 1
      else pascal(c, r - 1) + pascal(c - 1, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance_count(chars: List[Char], balance: Int): Boolean = 
        if ((chars.isEmpty) && (balance == 0)) true
        else if (balance < 0) false
        else chars.head match {
          case '(' => balance_count(chars.tail, balance + 1)
          case ')' => balance_count(chars.tail, balance - 1)
          case  _  => balance_count(chars.tail, balance)

        }
       
      balance_count(chars, 0)
    }
    
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
