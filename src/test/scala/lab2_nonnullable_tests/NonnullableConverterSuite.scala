package lab2_nonnullable_tests

import org.scalatest._

import common._
import common.Implicits.pair2production

import lab2_nonnullable.NonnullableConverter


trait TestGrammar {
  val S = Nonterminal("S")
  val A = Nonterminal("A")
  val B = Nonterminal("B")

  val S_ = Nonterminal("!S")
  val A_ = Nonterminal("!A")
  val B_ = Nonterminal("!B")

  val ta = Terminal("a")
  val tb = Terminal("b")

  val e = EmptySymbol

  val testGrammar = Grammar(
    terminals = Set(ta, tb),
    nonterms = Set(A, B, S),
    startSymbol = S,
    productions = Set(
      S -> Seq(A, B),
      A -> Seq(ta, A),
      A -> Seq(e),
      B -> Seq(tb, A),
      B -> Seq(e)
    )
  )
}

class NonnullableConverterSuite extends FunSuite with Matchers with TestGrammar {
  test("find nullables in example grammar") {
    val nc = new NonnullableConverter()
    val actual = nc.findNullables(testGrammar)
    val expected = Set(S, A, B)

    actual shouldEqual expected
  }

  test("split chain into B1..BiX1..Xi")  {
    val nc = new NonnullableConverter()
    val actual = nc.split(Seq(A, B, S, ta, B), Set(A, B))
    val expected = (Seq(A, B), Seq(S, ta, B))

    actual shouldEqual expected
  }

  test("split chain into B1..BiX1..Xi with m = 0")  {
    val nc = new NonnullableConverter()
    val actual = nc.split(Seq(S, ta, B), Set(A, B))
    val expected = (Seq(), Seq(S, ta, B))

    actual shouldEqual expected
  }

  test("split chain into B1..BiX1..Xi with n = 0")  {
    val nc = new NonnullableConverter()
    val actual = nc.split(Seq(A, B, B), Set(A, B))
    val expected = (Seq(A, B, B), Seq())

    actual shouldEqual expected
  }

  test("create new productions") {
    val nc = new NonnullableConverter()
    val nullables = Set(A, B, S)

    val actual = nc.createNewProductions(testGrammar, nullables, Map(A -> A_, B -> B_, S -> S_))
    val expected: Set[Production] = Set(
      S       -> Seq(A_, B),
      S       -> Seq(B_),
      S_      -> Seq(A_, B),
      S_      -> Seq(B_),
      A_      -> Seq(ta, A),
      A       -> Seq(ta, A),
      A       -> Seq(e),
      B_      -> Seq(tb, A),
      B       -> Seq(tb, A),
      B       -> Seq(e)
    )

    actual shouldEqual expected
  }

  test("convert grammar") {
    val nc = new NonnullableConverter()
    val expectedGrammar = Grammar(
      terminals = testGrammar.terminals,
      nonterms = testGrammar.nonterms ++ Set(A_, B_, S_),
      productions = Set(
        S       -> Seq(A_, B),
        S       -> Seq(B_),
        S_      -> Seq(A_, B),
        S_      -> Seq(B_),
        A_      -> Seq(ta, A),
        A       -> Seq(ta, A),
        A       -> Seq(e),
        B_      -> Seq(tb, A),
        B       -> Seq(tb, A),
        B       -> Seq(e)
      ),
      startSymbol = S_
    )

    val actualGrammar = nc.convert(testGrammar)

    actualGrammar shouldEqual expectedGrammar
  }
}
