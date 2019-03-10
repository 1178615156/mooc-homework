
import java.util.concurrent.ForkJoinTask

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

    override def toString: String = {
      0 until width map (x =>
        0 until height map (y =>
          apply(x, y).toString
          ) mkString (" ")
        ) mkString ("\n")
    }
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val l = for {
      xIndex <- (x - radius) to (x + radius) filter (_ >= 0) filter (_ < src.width)
      yIndex <- (y - radius) to (y + radius) filter (_ >= 0) filter (_ < src.height)
    } yield
      src(xIndex, yIndex)
    val _red = l map red
    val _green = l map green
    val _blue = l map blue
    val _alpha = l map alpha
    val size = l.size
    rgba(
      _red.sum / size,
      _green.sum / size,
      _blue.sum / size,
      _alpha.sum / size
    )
  }

  def joinTask(tasks: Seq[ForkJoinTask[Unit]]): Unit = {
    if (tasks.size <= 1)
      tasks.foreach(_.join())
    else {
      val size = tasks.size / 2
      parallel(joinTask(tasks.take(size)), joinTask(tasks.drop(size)))
    }
  }

}
