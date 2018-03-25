
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
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  /*def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var xr = -radius
    var yr = -radius
    var (ra, ga, ba, aa) = (0.0f,0.0f,0.0f,0.0f)
    var norm = 0.0f
    while (xr <= radius) {
      yr = -radius
      while (yr <= radius) {
        def xcl = clamp(x+xr, 0, src.width-1)
        def ycl = clamp(y+yr, 0, src.height-1)
        def rgba = src(xcl,ycl)

        ra = ra+ red(rgba)
        ga = ga+ green(rgba)
        ba = ba+ blue(rgba)
        aa = aa+ alpha(rgba)

        norm  = norm +1
        yr = yr+ 1
      }
      xr = xr+ 1
    }
    rgba(Math.ceil(ra/norm), Math.ceil(ga/norm), Math.ceil(ba/norm), Math.ceil(aa/norm))

  }*/
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var (r,g,b,a) = (0,0,0,0)

    var norm = 0

    var xi = clamp(x-radius, 0, src.width-1)
    while(xi<=clamp(x+radius, 0, src.width-1)) {
      var yi = clamp(y-radius, 0, src.height-1)
      while(yi<=clamp(y+radius, 0, src.height-1)) {
        r = r + red(src(xi, yi))
        g = g + green(src(xi, yi))
        b = b + blue(src(xi, yi))
        a = a + alpha(src(xi, yi))
        norm = norm + 1
        yi = yi + 1
      }
      xi = xi + 1
    }

    rgba(r/norm, g/norm, b/norm, a/norm)
  }

}
