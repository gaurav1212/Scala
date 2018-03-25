package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
      Gen.const(empty),
      for {
        num <- arbitrary[Int]
        hp <- genHeap
      } yield insert (num, hp)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("singleTon" ) = forAll { (i: Int) =>
      isEmpty(deleteMin(insert(i, empty)))
  }

  property("twoElemMin") = forAll { ( i: Int, j: Int) =>
    val Hp = insert(i, insert(j, empty))
    val k = findMin(Hp)
    k == Math.min(i,j)
  }

  property("sortedSeq") = forAll { (h: H) =>
    def extractSeq(hp: H) : List[Int] = {
      if(isEmpty(hp)) Nil
      else findMin(hp) :: extractSeq(deleteMin(hp))
    }
    val sq = extractSeq(h)
    sq == sq.sorted
  }

  property("mergeMin") = forAll { (h1: H, h2:H) =>
    if (isEmpty(h1) || isEmpty(h2)) {true }
    else {
      Math.min(findMin(h1), findMin(h2)) == findMin(meld(h1,h2))
    }
  }

  // get sorted seq of (h1,h2) by making min of one heap move to the other
  property("shuffle") = forAll { (h1: H, h2:H) =>
    if (isEmpty(h2)) {true }
    else {
      val M1 = meld(h1, h2)
      val M2 = meld(insert(findMin(h2), h1), deleteMin(h2))

      def extractSeq(hp: H): List[Int] = {
        if (isEmpty(hp)) Nil
        else findMin(hp) :: extractSeq(deleteMin(hp))
      }
      extractSeq(M1) == extractSeq(M2)
    }
  }
}
