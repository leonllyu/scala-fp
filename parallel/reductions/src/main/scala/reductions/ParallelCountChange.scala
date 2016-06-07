package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  //TODO: sequential
  def countChanges(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins == null || coins.isEmpty || money < 0) 0 //throw new Exception("no solution")
    else if (money < coins.head) countChanges(money, coins.tail)
    else countChanges(money - coins.head, coins) + countChanges(money, coins.tail)
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  //TODO: parallel
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (threshold(money, coins)) countChanges(money, coins)
    else if (money == 0) 1
    else if (coins == null || coins.isEmpty || money < 0) 0
    else if (money < coins.head) parCountChange(money, coins.tail, threshold)
    else {
      val (sum1, sum2) = parallel(parCountChange(money - coins.head, coins, threshold),
                            parCountChange(money, coins.tail, threshold))
      sum1 + sum2
    }
  }
  
  def countChange(money: Int, coins: List[Int]): Int = {
    //countChanges(money, coins)
    //parCountChange(money, coins, moneyThreshold(money))
    //parCountChange(money, coins, totalCoinsThreshold(coins.sum))
    parCountChange(money, coins, combinedThreshold(money,coins))
  }

  /** Threshold heuristic based on the starting money. */
  //TODO: one heuristic strategy for threshold 2/3 of the starting money
  def moneyThreshold(startingMoney: Int): Threshold =
    (currentMoney: Int, coins: List[Int]) => currentMoney <= startingMoney * 2 / 3

  /** Threshold heuristic based on the total number of initial coins. */
  //TODO: another heuristic strategy only on coins
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (currentMoney: Int, coins: List[Int]) => //coins.foldLeft(0)(_ + _) <=  totalCoins * 2 / 3
      coins.size <= totalCoins * 2 / 3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  //TODO:
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (currentMoney: Int, coins: List[Int]) => //currentMoney <= (startingMoney * 2 / 3) ||
              //(coins.foldLeft(0)(_ + _) <= allCoins.foldLeft(0)(_ + _) * 2 / 3)
              //coins.size <= allCoins.size * 2 / 3 &&
              currentMoney * coins.size <= startingMoney * allCoins.size / 2
  }
}
