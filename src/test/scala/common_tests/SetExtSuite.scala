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

  test("combinations of {1, 2, 3}") {
    val set = Set(1, 2, 3)
    val comb = set.combinations()
    val expected = Set(
      Set(1),
      Set(2),
      Set(3),
      Set(1, 2),
      Set(1, 3),
      Set(2, 3),
      Set(1, 2, 3)
    )

    val actual = set.combinations()

    actual shouldEqual expected
  }

  test("combinations of {1, 2, 3, 4}") {
    val set = Set(1, 2, 3, 4)
    val expected = Set(
      Set(1), Set(1, 2), Set(1, 3), Set(1, 2, 3), Set(1, 4), Set(1, 2, 3, 4),
      Set(1, 2, 4), Set(1, 3, 4),
      Set(2), Set(2, 3), Set(2, 4), Set(2, 3, 4),
      Set(3), Set(3, 4),
      Set(4)
    )

    val actual = set.combinations()

    actual shouldEqual expected
  }
}
