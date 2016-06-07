package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    pascal2.take(11).print()
  }
  
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    //if (c == 0 || c == r) 1
    //else pascal(c-1, r-1) + pascal(c, r-1);
    pascal2.apply(r).apply(c)

  val pascal1: Stream[Stream[Long]] = 
  (Stream(1L) 
    #:: (Stream from 1 map { i => 
      // compute row i
      (1L 
        #:: (pascal1(i-1) // take the previous row
               sliding 2 // and add adjacent values pairwise
               collect { case Stream(a,b) => a + b }).toStream 
        ++ Stream(1L))
    }))
    
  val pascal2 = Stream.iterate(Seq(1))(a=>(0+:a,a:+0).zipped.map(_+_))
  
  /**
   * Exercise 2
   */
  
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def countparentheses(count: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty) count == 0
      else chars.head match {
        case ')' => if (count == 0) false else countparentheses(count-1, chars.tail)
        case '(' => countparentheses(count+1, chars.tail)
        case _ => countparentheses(count, chars.tail)
      }

    countparentheses(0, chars)
  }

  /**
   * Exercise 3 - different implementations to make changes for an amount
   *              with coins unlimited and no duplicate
   */
  /**
   * simple recursion
   * 31.6 / 33.68 seconds for money 3000 with sorted coins (run two times)
   * Scala compiler provides tail recursive optimization, such that the compiled bytecode is not recursive.
   * But here, countChange1 is not pure tail recursive, so there might be recursion in the compiled bytecode.
   * print stack trace found out (replace the first condition with throw new Exception) that
   * several countChange1 were called in the stack trace.
   * In Scala 2.8, use @tailrec to mark specific method to hope the compiler will optimize
   * use annotation @tailrec would give error on the last  line of the function, since it's not tail recursive
   * 
   * How to implement tail recursive version?
   */

/*@tailrec*/  def countChange1(money: Int, coins: List[Int]): Int = 
    if (coins == null || coins.isEmpty || money < 0) 0 //throw new Exception("no solution")
    else if (money == 0) 1
    else if (money < coins.head) countChange1(money, coins.tail)
    else countChange1(money - coins.head, coins) + countChange1(money, coins.tail)

  /* this is even much slower --naive way, instead should consider CPS, async / promise, or stream
  def countChange1(money: Int, coins: List[Int]): Int = 
    if (coins == null || coins.isEmpty || money < 0) 0 //throw new Exception("no solution")
    else if (money == 0) 1
    else {
      lazy val tailpart = countChange1(money, coins.tail)
      if (money < coins.head) tailpart
    //else countChange1(money - coins.head, coins) + countChange1(money, coins.tail)
      else {
        lazy val wholepart = countChange1(money - coins.head, coins)
        wholepart + tailpart
      }
    }
    * 
    */

  /**
   * simple optimization with storing search results
   * 0.059 second for money 300 with sorted coins
   * 0.211 second for money 3000 with sorted coins
   */
  def countChange3(money: Int, coins: List[Int]): Int = {
    def process(money: Int, coins: List[Int], index: Int, moneyMap: Array[Array[Int]]): Int = {
      var res = 0
      if (index < coins.length) {
        var mapValue = 0
        var i = 0
        while (coins.apply(index) * i <= money) {
          mapValue = moneyMap(index + 1)(money - coins.apply(index) * i)
          if (mapValue > 0)
            res += mapValue
          else if (mapValue == 0)
            res += process(money - coins.apply(index) * i, coins, index + 1, moneyMap)
          //else == -1 do nothing
          i += 1
        }
        if (res == 0)
          moneyMap(index)(money) = -1
        else
          moneyMap(index)(money) = res
      }
      else if (money == 0)
             res = 1
      else   res = 0
      res
    }

    if (coins == null || coins.length == 0 || money < 0) 0
    else {
      var moneyMap = Array.ofDim[Int](coins.length + 1, money + 1)
      //var moneyMap = Array.fill[Int](coins.length + 1, money + 1) { new Int }
      //var moneyMap = Array.tabulate[Int](coins.length + 1, money + 1) { (i,j) = new Int(i, j) }
      process(money, coins, 0, moneyMap)    
    }
  }

  /**
   * raw dynamic programming version with enumeration - no recursion
   * 0.061 second for money 3000 with sorted coins
   * 0.428 second for money 30000 with sorted coins
   */
  def countChange4(money: Int, coins: List[Int]): Int = {
    if (coins == null || coins.length == 0 || money < 0) 0
    else {
      var moneyMap = Array.ofDim[Int](coins.length, money + 1)
      for (i <- 0 until coins.length) // or to coins.length - 1
        moneyMap(i)(0) = 1
      for (j <- 1 to money / coins.apply(0)) // floor
        moneyMap(0)(coins.apply(0) * j) = 1
  
      for (i <- 1 until coins.length)
        for (j <- 1 to money) {
          var num = 0
          for (k <- 0 to j / coins.apply(i)) // floor, yes it is
              num += moneyMap(i - 1)(j - coins.apply(i) * k)
          moneyMap(i)(j) = num
        }
      moneyMap(coins.length - 1)(money)
    }
  }

  /**
   * optimized dynamic programming version without enumeration - no recursion
   * similar to countChange1 without recursion
   * 0.008 second for money 3000 with sorted coins
   * 0.016 second for money 30000 with sorted coins
   */
  def countChange5(money: Int, coins: List[Int]): Int = {
    if (coins == null || coins.length == 0 || money < 0) 0
    else {
      var moneyMap = Array.ofDim[Int](coins.length, money + 1)
      for (i <- 0 until coins.length) // or to coins.length - 1
        moneyMap(i)(0) = 1
      for (j <- 1 to money / coins.apply(0)) // floor
        moneyMap(0)(coins.apply(0) * j) = 1
  
      for (i <- 1 until coins.length)
        for (j <-1 to money) {
          moneyMap(i)(j) = moneyMap(i - 1)(j)
          val col = j - coins.apply(i)
          if (col >= 0)
            moneyMap(i)(j) += moneyMap(i)(col)
        }
      moneyMap(coins.length - 1)(money)
    }
  }

  /**
   * optimized dynamic programming version without enumeration - no recursion
   * more optimized for space
   * similar to countChange1 without recursion
   * 0.008 second for money 3000 with sorted coins
   * 0.012 second for money 30000 with sorted coins
   */
  def countChange6(money: Int, coins: List[Int]): Int = {
    if (coins == null || coins.length == 0 || money < 0) 0
    else {
      var moneyMap = Array.ofDim[Int](money + 1)
      for (j <- 0 to money / coins.apply(0)) // floor
        moneyMap(coins.apply(0) * j) = 1
      for (i <- 1 until coins.length)
        for (j <-1 to money) {
          val col = j - coins.apply(i)
          if (col >= 0)
            moneyMap(j) += moneyMap(col)
        }      
        moneyMap(money)
    }
  }

  def countChange(money: Int, coins: List[Int]): Int = countChange1(money, coins)
  /*
  def countChange(money: Int, coins: List[Int]): Int = {
    val listCountChange = List(countChange3(_, _), countChange4(_, _),
                               countChange5(_, _), countChange6(_, _),
                               countChange1(_, _))
    var cC = 0
    for (cc <- listCountChange) {
      if (money < 3000){
        cC = cc(money, coins)
        println(cC)
      }
      else if (cc != listCountChange.reverse.head){//does not work! if (cc != countChange1(money, coins)){
        cC = cc(money, coins)
        println(cC)
      }
    }
    cC
  }
  * 
  */
  
  /**
   * Extra Exercise a - different implementations to minimum number of coins for change
   *                    with coins unlimited and no duplicate
   * return
   *         0: no coins needed when money is 0
   *         -1: no way to find changes
   *         min: there are changes
   *
   * 32.5 seconds for money of 3475
   */
  def minCoinChanges1(money: Int, coins: List[Int]): Int = {
    //val MaxInt: Int = 1<<30
    /*@tailrec*/ def findMin(money: Int, coins: List[Int]): Int = // this is still not tail-recursive
      if (money == 0) 0
      else if (coins == null || coins.isEmpty || money < 0) Int.MaxValue
      else if (money < coins.head) findMin(money, coins.tail)
      else Math.min(findMin(money - coins.head, coins)+1, findMin(money, coins.tail))

    val tmp = findMin(money, coins)
    if (tmp == scala.Int.MaxValue) -1
    else tmp
  }
  
  /**
   * Extra Exercise b - different implementations to minimum number of coins for change
   * where list of coins has exact money to be selected (rather than unlimited) and might be duplicate
   * return
   *         0: no coins needed when money is 0
   *         -1: no way to find changes
   *         min: there are changes
   * no need to do dynamic programming for better performance
   * complexity is rational to the coins length
   */
  def minCoins1(money: Int, coins: List[Int]): Int = {
    /*@tailrec*/ def findMin(money: Int, coins: List[Int]): Int = // this is still not tail-recursive
      if (money == 0) 0
      else if (coins == null || coins.isEmpty || money < 0) Int.MaxValue
      else if (money < coins.head) findMin(money, coins.tail)
      else Math.min(findMin(money - coins.head, coins.tail)+1, findMin(money, coins.tail))

    val tmp = findMin(money, coins)
    if (tmp == scala.Int.MaxValue) -1
    else tmp
  }
}
