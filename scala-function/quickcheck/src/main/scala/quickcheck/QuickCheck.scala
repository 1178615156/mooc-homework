package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.Try

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = Gen.oneOf(List(
    empty
    , insert(0, empty)
    , insert(1, empty)
    , insert(2, empty)
    , insert(0, insert(1, empty))
    , insert(0, insert(0, empty))
    , insert(0, insert(1, insert(3, empty)))

  ))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  property("x") = forAll((h: H) => {
    if (!isEmpty(h)) {
      findMin(h) <= Try(findMin(deleteMin(h))).getOrElse(Int.MaxValue)
    } else {
      Try(deleteMin(h)).isFailure
    }
  })

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  property("min2") = forAll { a: List[Int] =>
    if (a.nonEmpty) {
      val m = a.min
      val h = a.foldLeft(empty) { case (h, e) =>
        insert(e, h)
      }
      findMin(h) == m
    } else
      true
  }
  property("delete min") = forAll { a: List[Int] =>
    if (a.nonEmpty) {
      val h = a.sorted.foldLeft(empty) { case (h, e) =>
        insert(e, h)
      }
      a.sorted.foldLeft(h -> true) {
        case ((h, r), m) =>
          deleteMin(h) -> (r && findMin(h) == m)
      }._2

    } else
      true

  }
}
