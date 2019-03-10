package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

//@RunWith(classOf[JUnitRunner])
class LineOfSightSuite extends FunSuite {

  import LineOfSight._

  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }
  test("upsweep") {
    1 to 100 foreach (i => {
      upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4) ===
        upsweep(Array[Float](0f, 1f, 8f, 9f), 1, 4, i).maxPrevious
    })
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }
  test("down sweep"){
    val inout  = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](4)

    downsweep(inout, output,0f,upsweep(inout,0,inout.size,2))
    assert(output.toList == List(0f, 1f, 4f, 4f))

  }

  test("down sweep xz"){
    val inout  = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](4)

    downsweep(inout, output,2f,upsweep(inout,0,inout.size,2))
    assert(output.toList == List(0f, 1f, 4f, 4f))

  }

}

