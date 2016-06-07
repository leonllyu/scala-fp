
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
    
    //def w: Int = width
    //def h: Int = height
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    type Quadruple = (Int, Int, Int, Int)
    val numPixels = (clamp(x+radius, 0, src.width-1)-clamp(x-radius, 0, src.width-1)+1) * 
                      (clamp(y+radius, 0, src.height-1)-clamp(y-radius, 0, src.height-1)+1)
    def tupleAdd(x: Quadruple, y: Quadruple): Quadruple =
      (x._1 + y._1, x._2 + y._2, x._3 + y._3, x._4 + y._4)
    def rgbaComputation(): Int = {
      val rgbaSeq = for {
        i <- clamp(x-radius, 0, src.width-1) to clamp(x+radius, 0, src.width-1)
        j <- clamp(y-radius, 0, src.height-1) to clamp(y+radius, 0, src.height-1)
      } yield (red(src(i, j)), green(src(i, j)), blue(src(i, j)), alpha(src(i, j)))
      
      //rgbaSeq.foldLeft(0)(_ + _)
      // the following works, but there are four maps
      //rgba(rgbaSeq.map(_._1).sum / numPixels, rgbaSeq.map(_._2).sum / numPixels,
      //    rgbaSeq.map(_._3).sum / numPixels, rgbaSeq.map(_._4).sum / numPixels)
      //val rgbaReduced = rgbaSeq.reduce(tupleAdd)
      //use parallel collection instead
      val rgbaReduced = rgbaSeq.par.reduce(tupleAdd)
      rgba(rgbaReduced._1/numPixels,rgbaReduced._2/numPixels,rgbaReduced._3/numPixels,rgbaReduced._4/numPixels)
    }
    
    def rgbaParComputation(rgba: RGBA => Int): Int = {
      /*
      val result = 0
      for (int i <- x - radius; i < x + radius; i++)
        for (int j <- y - radius; i < y + radius; j++)
              result += rgba(src(i, j))
      result
      * 
      */
    
      /*
      ((clamp(x-radius, 0, src.w) to clamp(x+radius, 0, src.w)) map (i =>
        (clamp(y-radius, 0, src.h) to clamp(y+radius, 0, src.h)
      //(x-radius to x+radius).flatMap(i =>
      //  (y-radius to y+radius)
        map(j => rgba(src(i, j))))
        foldLeft(0)(_ + _)
      * 
      */
      
      val rgbaSeq = for {
        i <- clamp(x-radius, 0, src.width) to clamp(x+radius, 0, src.width)
        j <- clamp(y-radius, 0, src.height) to clamp(y+radius, 0, src.height)
      } yield rgba(src(i, j))
      
      rgbaSeq.foldLeft(0)(_ + _)
    }
    
    /* do not use parallel
    val redComputation = task { rgbaParComputation(red) }
    val greenComputation = task { rgbaParComputation(green) }
    val blueComputation = task { rgbaParComputation(blue) }
    val alphaComputation = task { rgbaParComputation(alpha) }
    
    redComputation.join()
    greenComputation.join()
    blueComputation.join()
    alphaComputation.join()
    
    val resultRGBA = rgba(redComputation.get() / numPixels,
                          greenComputation.get() / numPixels,
                          blueComputation.get() / numPixels,
                          alphaComputation.get() / numPixels)
                          
    //src.update(x, y, resultRGBA)
    resultRGBA
    * 
    */

    //rgba(rgbaComputation(red)/numPixels, rgbaComputation(green) / numPixels,
    //     rgbaComputation(blue) / numPixels, rgbaComputation(alpha) / numPixels)
    rgbaComputation()
  }
}
