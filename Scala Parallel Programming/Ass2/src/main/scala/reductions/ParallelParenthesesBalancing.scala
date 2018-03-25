package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceAcc(chars: Array[Char], from: Int, balSoFar: Int): Boolean = {
      if (from >= chars.length) {
        balSoFar == 0
      }
      else if (balSoFar < 0) {
        false
      }
      else {
        if (chars(from) == '(') balanceAcc(chars, from + 1, balSoFar + 1)
        else if (chars(from) == ')') balanceAcc(chars, from + 1, balSoFar - 1)
        else balanceAcc(chars, from + 1, balSoFar)
      }
    }
    balanceAcc(chars, 0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      var pos=idx
      var opens = arg1
      var closes = arg2
      while (pos < until) {
        if (chars(pos) == '(') {
          opens = opens + 1
        }
        else if (chars(pos) == ')') {
          if (opens > 0) {
            opens  = opens - 1
          }
          else {
            closes = closes + 1
          }
        }
        pos = pos + 1
      }
      (opens, closes)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      }
      else {
        val mid = (from+until)/2
        val ((a1l, a2l), (a1r, a2r)) = parallel(reduce(from, mid), reduce(mid, until))
        val common = math.min(a1l, a2r)
        (a1l+a1r-common, a2l+a2r-common)
      }

    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
