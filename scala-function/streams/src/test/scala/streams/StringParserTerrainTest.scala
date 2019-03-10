package streams

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/6/21.
  */
class StringParserTerrainTest extends FunSuite {

  test("test") {
    val spt =
      new StringParserTerrain {
        val level =
          """ST
            |oo
            |oo""".stripMargin
      }
    import spt.Pos
    assert(
      spt.goal === Pos(0, 1)
    )
    for{
      x <- 0 to 1
      y <- 0 to 1
    }{
      assert(spt.terrain(Pos(x,y)))
    }

    for {
      x <- 2 to 10
      y <- 2 to 10
    }{
      assert(!spt.terrain(Pos(x,y)))

    }
  }
}
