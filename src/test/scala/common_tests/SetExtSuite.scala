package common_tests

import org.scalatest.{Matchers, FunSuite}

import common.SetExt._


class SetExtSuite extends FunSuite with Matchers {
  test("cartesion product of {1, 2} and {orange, apple} is" +
       "{(1, apple), (1, orange), (2, apple), (2, orange)}") {
    val numbers = Set(1, 2)
    val fruits = Set("apple", "orange")

    val actual = numbers cartesianProduct fruits
    val expected = Set(
      (1, "apple"), (1, "orange"), (2, "apple"), (2, "orange")
    )

    actual shouldEqual expected
  }
}
