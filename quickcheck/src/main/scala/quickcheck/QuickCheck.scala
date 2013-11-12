package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val m = if (a < b) a else b
    findMin(h) == m
  }

  property("del1") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  property("sort1") = forAll { l: List[Int] =>
    def elements(h: H): List[Int] = {
      if (isEmpty(h)) Nil
      else {
        findMin(h) :: elements(deleteMin(h))
      }
    }
    def heap(l: List[Int], h: H): H = l match {
      case Nil => h
      case x :: xs => heap(xs, insert(x, h))
    }

    val sorted = l.sorted(ord)

    val h = heap(l, empty)

    sorted == elements(h)
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(value(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
