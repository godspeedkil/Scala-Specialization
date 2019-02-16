package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      n <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(n, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert into empty>delete>get empty") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  } // checked

  property("insert into empty>return inserted") = forAll { a: Int =>
    findMin(insert(a, empty)) == a
  } // checked

  property("min of 2-heap is min of two") = forAll { (a: Int, b:Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == List(a, b).min
  }

  def heapToList(h: H): List[Int] =
    if (isEmpty(h)) List()
    else findMin(h) :: heapToList(deleteMin(h))

  property("finding minima returns sorted list") = forAll { h: H =>
    val minima = heapToList(h)
    minima.sorted == minima
  } // checked

  property("minimum of meld is one of the two minima") = forAll { (h1: H,
    h2: H) =>
      val h3 = meld(h1, h2)
      if (isEmpty(h3)) true
      else if (isEmpty(h1)) findMin(h3) == findMin(h2)
      else if (isEmpty(h2)) findMin(h3) == findMin(h1)
      else {
        val min3 = findMin(h3)
        min3 == findMin(h1) || min3 == findMin(h2)
      }
  }

  property("heapToList contains everything") = forAll { (a: Int, h: H) =>
    val h1 = insert(a, h)
    heapToList(h1) contains a
  }

}
