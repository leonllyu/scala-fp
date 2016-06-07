import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  //TODO:
  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0.0f
    def total: Int = 0
    def insert(b: Body): Quad = {
      val massX = b.x
      val massY = b.y
      val mass = b.mass
      val total = 1     
      new Leaf(centerX, centerY, size, Seq(b))
    }
  }

  //TODO:
  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = (nw.centerX + ne.centerX) / 2
    val centerY: Float = (nw.centerY + sw.centerY) / 2
    val size: Float = math.max(nw.centerX - ne.centerX, sw.centerY - nw.centerY) * 2
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = if (mass == 0) centerX else (nw.mass * nw.massX + ne.mass * ne.massX + sw.mass * sw.massX + se.mass * se.massX) / mass
    val massY: Float = if (mass == 0) centerY else (nw.mass * nw.massY + ne.mass * ne.massY + sw.mass * sw.massY + se.mass * se.massY) / mass

    val total: Int = nw.total + ne.total + sw.total + se.total

    //TODO:
    def insert(b: Body): Fork = {
      if (b.x <= centerX){
        if (b.y <= centerY) new Fork(nw.insert(b), ne, sw, se)
        else new Fork(nw, ne, sw.insert(b), se)
      }
      else {
        if (b.y <= centerY) new Fork(nw, ne.insert(b), sw, se)
        else new Fork(nw, ne, sw, se.insert(b))
      }
    }
  }

  //TODO:
  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    def mass = bodies.foldLeft(0.0f)((m, b) => m + b.mass)
    def massX = bodies.foldLeft(0.0f)((m, b) => m + b.mass * b.x) / mass
    def massY = bodies.foldLeft(0.0f)((m, b) => m + b.mass * b.y) / mass
    def total: Int = bodies.length
    def insert(b: Body): Quad =
      if (size > minimumSize){
        var newFork = new Fork(Empty(centerX+size/4, centerY-size/4, size/2), Empty(centerX-size/4, centerY-size/4, size/2),
                        Empty(centerX+size/4, centerY+size/4, size/2), Empty(centerX-size/4, centerY+size/4, size/2))
        bodies.foldLeft(newFork)(_.insert(_))
        newFork = newFork.insert(b)
        newFork
      }
      else new Leaf(centerX, centerY, size, bodies :+ b)
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      //TODO:
      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) => {
          // add force contribution of each body by calling addForce
          bodies.foreach(b => {
            val dist = distance(b.x, b.y, x, y)
            val dforce = force(mass, b.mass, dist)
            val xn = (b.x - x) / dist
            val yn = (b.y - y) / dist
            val dforcex = dforce * xn
            val dforcey = dforce * yn
            netforcex += dforcex
            netforcey += dforcey
          })
        }

        case Fork(nw, ne, sw, se) => {
          // if far away, it's opaque
          if (math.sqrt((quad.massX-x)*(quad.massX-x)+(quad.massY-y)*(quad.massY-y))<=eliminationThreshold*quad.size) {
          // see if node is far enough from the body,
          // or recursion is needed
          if (nw.size / distance(nw.massX, nw.massY, x, y) < theta) addForce(nw.mass, nw.massX, nw.massY)
          else traverse(nw)

          if (ne.size / distance(ne.massX, ne.massY, x, y) < theta) addForce(ne.mass, ne.massX, ne.massY)
          else traverse(ne)

          if (sw.size / distance(sw.massX, sw.massY, x, y) < theta) addForce(sw.mass, sw.massX, sw.massY)
          else traverse(sw)

          if (se.size / distance(se.massX, se.massY, x, y) < theta) addForce(se.mass, se.massX, se.massY)
          else traverse(se)
          }
        }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }
  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    //TODO:
    def +=(b: Body): SectorMatrix = {
      //for out of boundary insert into closest
      var m = 0
      var n = 0
      if (b.x > boundaries.maxX) m = 0
      if (b.x < boundaries.minX) m = sectorPrecision - 1
      if (b.y < boundaries.minY) n = 0
      if (b.y > boundaries.maxY) n = sectorPrecision - 1
      if (b.x <= boundaries.maxX && b.x >= boundaries.minX) {
        m = math.round((b.x - boundaries.minX) / sectorSize - 0.5f)
      }
      if (b.y <= boundaries.maxY && b.x >= boundaries.minY) {
        n = math.round((b.y - boundaries.minY) / sectorSize - 0.5f)
      }
      matrix(m*sectorPrecision + n)+=(b)
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    //TODO:
    def combine(that: SectorMatrix): SectorMatrix = {
      for {
        i <- 0 until sectorPrecision
        j <- 0 until sectorPrecision
      } yield matrix(i*sectorPrecision + j).combine(that.apply(i, j))
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
