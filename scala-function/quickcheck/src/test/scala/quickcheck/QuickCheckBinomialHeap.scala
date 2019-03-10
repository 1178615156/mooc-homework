package quickcheck

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._

import org.scalatest.exceptions.TestFailedException

/**
  * Created by yujieshui on 2016/6/22.
  */
object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap {



}
