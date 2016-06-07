package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
  // TODO implement this method using the `boxBlurKernel` method
    def blurStrip(src: Img, dst: Img, strip: Int, radius: Int): Unit = {
      (0 until src.width) map (x => dst.update(x, strip, boxBlurKernel(src, x, strip, radius)))
    }
    
    (from until end) map (y => blurStrip(src, dst, y, radius))
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
  // TODO implement using the `task` construct and the `blur` method
    
    // if src.height < numTasks, more than one tasks might do the same thing here!
    // instead, start the number of src.height tasks, each works on one row
    val stripHeight = src.height / numTasks;
    val selfDoing = src.height - stripHeight * numTasks;
    
    val tasks = (0 until numTasks) map (i => task {blur(src, dst, i*stripHeight, (i+1)*stripHeight, radius)})
    //no need to join, since they are independent and their results are not needed
    //no! main thread might finish and exit before the tasks finish
    
    //this main thread does the remaining rows, if there is
    if (selfDoing != 0) blur(src, dst, stripHeight*numTasks, src.height, radius)
    
    tasks.map(_.join())
  }
}
