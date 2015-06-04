package grammar_transform_tests

import common.{Grammar, Terminal, Nonterminal}
import common.Implicits._
import grammar_transforms.OneTerminalConverter

import org.scalatest.{Matchers, FunSuite}

class OneTerminalConverterSuite extends FunSuite with Matchers {
  val nS = Nonterminal("S")
  val nA = Nonterminal("A")
  val nB = Nonterminal("B")

  val ta = Terminal("a")
  val tb = Terminal("b")

  val testGrammar = Grammar(
    terminals = Set(ta, tb),
    nonterms = Set(nS, nA, nB),
    startSymbol = nS,
    productions = Set(
      nS -> Seq(nA, tb),
      nA -> Seq(ta, tb)
    )
  )

  test("works with example grammar") {
    val otA = Nonterminal("ONE_TERM_A")
    val otB = Nonterminal("ONE_TERM_B")

    val expected = testGrammar.copy(
      nonterms = testGrammar.nonterms ++ Set(otA, otB),
      productions = Set(
        nS -> Seq(nA, tb),
        nA -> Seq(otA, ta),
        nA -> Seq(otB, tb),
        otA -> Seq(ta),
        otB -> Seq(tb)
      )
    )

    val tr = new OneTerminalConverter()
    val actual = tr(testGrammar)

    actual shouldEqual expected
  }
}
