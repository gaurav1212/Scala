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
    val width = 1920
    val height = 1080
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
    for(x <- from until end){
      for(y <- 0 until src.height){
        if (x < src.width && y < src.height) {
          dst.update(x,y,boxBlurKernel(src, x, y, radius))
        }
      }
    }
  }
  /*
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var c = from
    while (c < end) {
      var r = 0
      while (r < src.height) {
        dst.update(r,c,boxBlurKernel(src,r,c,radius))
        r = r+1
      }
      c= c+ 1
    }
  }*/

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  /*
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {

    var currCol = 0
    var currTask = 0
    var numColsPerTask = src.width/numTasks
    //var tasks : List[Any] = List()
    while (currTask < numTasks) {
      var endCol = currCol+numColsPerTask
      if (currTask == numTasks - 1) {
        endCol = src.width
      }
      def t = task { blur (src, dst, currCol, endCol, radius)}
      //tasks = t::tasks
      currCol = endCol
      currTask = currTask + 1
    }

    /*while (!tasks.isEmpty) {
      tasks.head.asInstanceOf[Task[Any]].join()
      tasks = tasks.tail
    }*/

  }*/
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    var i = 0 to (src.width + (if(src.width%numTasks != 0 )  numTasks-src.width%numTasks else 0)) by (src.width/(Math.min(numTasks, src.width)))
    var intervals = i zip i.tail
    val tasks = intervals.map( { case (from, to) => task(blur(src, dst, from, to, radius)) } )
    tasks foreach {_.join}
  }

}
