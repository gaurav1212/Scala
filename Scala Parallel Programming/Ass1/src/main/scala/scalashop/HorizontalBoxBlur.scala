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
    for(y <- from until end){
      for(x <- 0 until src.width){
        if (x < src.width && y < src.height) {
          dst.update(x,y,boxBlurKernel(src, x, y, radius))
        }
      }
    }
  }
  /*
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var r = from
    while (r < end) {
      var c = 0
      while (c < src.width) {
        dst.update(r,c,boxBlurKernel(src,r,c,radius))
        c =c+1
      }
      r=r+1
    }
  }*/

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  /*
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    var currRow = 0
    var currTask = 0
    var numRowsPerTask = src.height/numTasks
    //var tasks : List[ForkJoinTask[Any]] = List()
    while (currTask < numTasks) {
      var endRow = currRow+numRowsPerTask
      if (currTask == numTasks - 1) {
        endRow = src.height
      }
      def t = task { blur (src, dst, currRow, endRow, radius)}.join()
      //tasks = t::tasks
      currRow = endRow
      currTask = currTask + 1
    }

    /*while (!tasks.isEmpty) {
      tasks.head.join()
      tasks = tasks.tail
    }*/

  }*/
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    var i = 0 to (src.height + (if(src.height%numTasks != 0 )  numTasks-(src.height%numTasks)  else 0)) by (src.height/(Math.min(numTasks, src.height)))
    var intervals = i zip i.tail
    val tasks = intervals.map( { case (from, to) => task(blur(src, dst, from, to, radius)) } )
    tasks foreach {_.join}
  }

}
