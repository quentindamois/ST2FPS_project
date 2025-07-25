package utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FunctionAndListParameter extends AnyFunSuite with Matchers{
  test("ListParam should be able to give a list of element to a curried function") {
    val testList = List(2, "a")
    val curriedSum: Int => String => String = x => y => x.toString + y
    val listParam = ListParam.fromList(testList.reverse)
    listParam.foldFromHead(curriedSum) shouldBe "2a"
  }
}
