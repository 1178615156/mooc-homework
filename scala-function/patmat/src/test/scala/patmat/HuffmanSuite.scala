package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

//@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 =
      Fork(
        Fork(
          Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5),
        Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  val tt = new TestTrees {}

  import tt._

  test("decode") {
    assert(decode(t1, List(1)) === "b".toList)
    assert(decode(t1, List(0)) === "a".toList)
    assert(decode(t1, List(0, 0)) === "aa".toList)
    assert(decode(t1, List(1, 1)) === "bb".toList)

    assert(decode(t2,List(1)) === "d".toList)
    assert(decode(t2,List(1,1)) === "dd".toList)
    assert(decode(t2,List(0,0)) === "a".toList)
    assert(decode(t2,List(0,0,1,1)) === "add".toList)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }


  test("createCodeTree(someText)") {
    val t =       createCodeTree("someText".toList)
    println(t )
    println(
      encode(t)("someText".toList)
    )
  }
  test("encode(t1)(\"ab\".toList)") {
    println(
      encode(t1)("ab".toList)
    )
  }
  test("decodedSecret") {
    println(
      decodedSecret.mkString("")
    )
  }
  test("encoder"){
println(
  encode(t1)("ab".toList)

)
  }
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  test("make code"){
    println(createCodeTree("aabbb".toList))
    println(createCodeTree("aabbbdddd".toList))
  }


}
