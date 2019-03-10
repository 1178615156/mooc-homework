package mook.probability
import Probability._
/**
  * Created by yujieshui on 2017/4/15.
  */
class ProbabilityTest extends org.scalatest.FunSuite {

  test("variance"){
    variance(Seq(
      1.0 -> 0.1,
      1.0 -> 0.9
    ))
  }
}
