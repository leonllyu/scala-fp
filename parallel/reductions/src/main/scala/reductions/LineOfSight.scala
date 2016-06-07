package reductions

import common._

import org.scalameter.Key
import org.scalameter.Warmer
import org.scalameter.config

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  //TODO: sequential
  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    
    var i = 1;
    var previousMax = Float.MinValue
    while (i < input.size) {     
      previousMax = max(previousMax, input(i) / i)
      output(i) = previousMax
      //println(output(i))
      i += 1
    }
    //(1 until input.size).foreach(Float.MinValue)((previousMax, i) => output(i) = max(previousMax, input(i) / i))
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  //TODO:
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    
    var i = from;
    var maxAngle = Float.MinValue
    while (i < until) {
      if (i != 0)
      maxAngle = max(maxAngle, input(i) / (i * 1f))
      i += 1
    }
    maxAngle
    //the following generates stack overflow
    //(from until until).foldLeft(Float.MinValue)((previousMax, i) => max(previousMax, input(i) / i))
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  //TODO:
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if (end - from <= threshold) new Leaf(from, end, upsweepSequential(input, from, end))
    else {
      val mid = (end - from) / 2 + from
      val (left, right) = parallel(upsweep(input, from, mid, threshold),
                                  upsweep(input, mid, end, threshold))
      new Node(left, right)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  //TODO:
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    var i = from;
    var maxAngle = startingAngle
    while (i < until) {
      if (i != 0)
        maxAngle = max(maxAngle, input(i) / (i * 1f))
      output(i) = maxAngle
      i += 1
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  //TODO:
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = tree match {
      case Leaf(from, until, maxPrevious) => {
        //if (startingAngle > maxPrevious) 
        //maxPrevious should not be used here, since that's the max of the current range
          downsweepSequential(input, output, startingAngle, from, until)
        //else 
        //  downsweepSequential(input, output, maxPrevious, from, until)
      }
      case Node(left, right) => {
        //left.maxPrevious has already been set in upsweep
        //so right does not need to wait for left, both left and right can do parallel
        //this is the essence of the solution to this kind of problems
        //that's why upsweep step is needed to get the sub-partial solutions
        //the only requirement is that the computation (here max) should be associative
        parallel(downsweep(input, output, startingAngle, left),
            downsweep(input, output, max(startingAngle, left.maxPrevious), right))
      }
    }

  /** Compute the line-of-sight in parallel. */
  //TODO:
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val tree = upsweep(input, 1, input.size, threshold)
    downsweep(input, output, Float.MinValue, tree)
  }
}
