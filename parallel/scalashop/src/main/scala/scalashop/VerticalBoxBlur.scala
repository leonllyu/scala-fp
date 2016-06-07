package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920 / 5
    val height = 1080 / 5
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    // this is sequential
    def updateWithPrint(src: Img, dst: Img, x: Int, y: Int, radius: Int): Unit = {
      (0 until src.width) flatMap (x =>
        (0 until src.height) map(y => print(" " + src(x, y) + " ")))
        
      println()
      println(x+":"+y+":"+boxBlurKernel(src, x, y, radius));
      dst.update(x, y, boxBlurKernel(src, x, y, radius));
      println(x+":"+y+":"+dst(x, y))
    }
    
    def blurStrip(src: Img, dst: Img, strip: Int, radius: Int): Unit = {
      (0 until src.height) map (y => //updateWithPrint(src, dst, strip, y, radius))
        dst.update(strip, y, boxBlurKernel(src, strip, y, radius)))
    }
    
    //(from until end) map (x => blurStrip(src, dst, x, radius))
    
    //use parallel collection instead
    (from until end).par map (x => blurStrip(src, dst, x, radius))
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    // this is parallel
    // when src.widht < numTasks, start the number of src.width tasks, each works on one column!
    val stripWidth = src.width / numTasks;
    val selfDoing = src.width - stripWidth * numTasks;
    
    val tasks = (0 until numTasks) map (i => task {blur(src, dst, i*stripWidth, (i+1)*stripWidth, radius)})
    //no need to join, since they are independent and their results are not needed
    //no! main thread might finish and exit before these tasks
    
    //this main thread does the remaining columns, if there is
    if (selfDoing != 0) blur(src, dst, stripWidth*numTasks, src.width, radius)
    
    tasks.map(_.join())
  }

}
