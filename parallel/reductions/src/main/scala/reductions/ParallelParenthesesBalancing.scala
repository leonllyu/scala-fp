package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  //TODO: sequential
  def balances(chars: Array[Char]): Boolean = {
    @tailrec
    def countparentheses(count: Int, chars: Array[Char]): Boolean =
      if (chars.isEmpty) count == 0
      else chars.head match {
        case ')' => if (count == 0) false else countparentheses(count-1, chars.tail)
        case '(' => countparentheses(count+1, chars.tail)
        case _ => countparentheses(count, chars.tail)
      }

    countparentheses(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    type Intuple = (Int, Int)
    //TODO: traverse array chars from index from until until
    // arg1 for right parentheses and arg2 for left parentheses
    @tailrec
    def traverse(from: Int, until: Int, arg1: Int, arg2: Int): Intuple = {
      
      if (from == until) (arg1, arg2)
      else chars(from) match {
        case ')' => if (arg2 > 0) traverse(from+1, until, arg1, arg2-1) 
                    else traverse(from+1, until, arg1+1, arg2)
        case '(' => traverse(from+1, until, arg1, arg2+1)
        case _ => traverse(from+1, until, arg1, arg2)
      }
      /*
      var arg1 = 0
      var arg2 = 0
      (idx until until) map (i => chars(i) match {
        case ')' => arg1+=1
        case '(' => arg2+=1
      })
      (arg1, arg2)
      * 
      */
    }

    //TODO: use two counters for right parentheses and left parentheses
    def reduce(from: Int, until: Int): Intuple = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (until - from ) / 2 + from
        val ((rightp1, leftp1), (rightp2, leftp2)) = 
          parallel (reduce(from, mid), reduce(mid, until))
        // merge parentheses in the adjunct sections
        if (leftp1 > rightp2) (rightp1, leftp1 - rightp2 + leftp2)
        else (rightp1 + rightp2 - leftp1, leftp2)
      }
    }

    val result = reduce(0, chars.length)// == (0, 0) // tuple provides ==?
    result._1 == 0 && result._2 == 0
  }

  def balance(chars: Array[Char]): Boolean = {
    //balances(chars)
    parBalance(chars, chars.length / 20 + 1) // threshold might be 0 if length < 2
  }
    
  // For those who want more:
  // Prove that your reduction operator is associative!

}
