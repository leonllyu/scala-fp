package kmeans

import scala.annotation.tailrec
import scala.collection._
import scala.util.Random
import org.scalameter._
import common._
/*
//for ScalaCheck
import org.scalacheck._
import Arbitrary._
import org.scalacheck.Gen._
//import org.scalameter.Gen._//???
import Prop._
* 
*/



class KMeans {

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to[mutable.ArrayBuffer]
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to[mutable.ArrayBuffer]
  }

  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    assert(means.size > 0)
    var minDistance = p.squareDistance(means(0))
    var closest = means(0)
    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    closest
  }

  //TODO: put a point to its closest class specified by mean
  //sequential, no need to parallel, points and means are collections to be parS
  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
    //some means might be empty
    var clusters: mutable.Map[Point,GenSeq[Point]]  = mutable.Map()//for { m <- means } yield (m, [GenSeq[Point]]GenSeq())
    means.foreach { m => 
      clusters += m -> [GenSeq[Point]]GenSeq()
    }
    /* this version does not work all the time, points missing sometimes
    points.foreach { p => {
      val closest: Point = findClosest(p, means)
      val newValue: GenSeq[Point] = if (clusters contains closest) (clusters(closest) :+ (p)) else GenSeq(p)
      clusters += closest ->  newValue
    }}
    clusters.toMap
    * 
    */
    
    clusters ++ points.groupBy(p => findClosest(p, means))
  }
  
  //parallel can also be used here
  // no need to do parallel explicitly, 
  //system do parallel on all Gen collections (GenSeq, GenMap) as much as possible - data parallel
  //or simply to convert data collection into parallel ones with .par
  //def parClassify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
  //  
  //}
  
  //def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = 
  //  classifies(points, means)

  def findAverage(oldMean: Point, points: GenSeq[Point]): Point = if (points.length == 0) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  //TODO: sequential
  def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    oldMeans.map { oldMean => //old mean
      findAverage(oldMean, classified(oldMean)) // new mean
    }
  }
  
  //TODO: parallel here, update each class independently
  // use task and user parallel
  // no need to do parallel - data parallel with par on collections
  def parUpdate(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    val tasks = oldMeans.map { oldMean => //old mean
      task {
        findAverage(oldMean, classified(oldMean)) // new mean
      }
    }
    tasks.map(_.join())
    //how to get result of task? -join would return the result when it's done.
  }
  
  /*
  def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    updates(classified, oldMeans)
    //parUpdate(classified, oldMeans)
  }
  * 
  */

  //TODO:
  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    var i = 0
    while (i < oldMeans.size) {    
      if (oldMeans(i).squareDistance(newMeans(i)) > eta) return false
        i += 1
    }
    true
  }

  @tailrec
  //kMeans is recursive, classify is called in each round, this is required
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    val updated = update(classify(points, means), means) // generate new means based on old means after new classify
    if (!converged(eta)(means, updated)) kMeans(points, updated, eta) else updated 
    // your implementation need to be tail recursive
  }
}

/** Describes one point in three-dimensional space.
 *
 *  Note: deliberately uses reference equality.
 */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      val parPoints = points.par
      val parMeans = means.par
      kMeans.kMeans(parPoints, parMeans, eta)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}
/*
class QuickCheckKMeans extends Properties("Point") {
}
* 
*/
