package grammar_transform_tests

import common.{Grammar, Nonterminal, EmptySymbol, Terminal}
import common.Implicits._
import grammar_transforms.UselessSymbolsEliminator

import org.scalatest.{Matchers, FunSuite}

class UselessSymbolsEliminatorSuite extends FunSuite with Matchers {
  val ta = Terminal("a")
  val tb = Terminal("b")
  val tc = Terminal("c")

  val eps = EmptySymbol

  val nS = Nonterminal("S")
  val nA = Nonterminal("A")
  val nB = Nonterminal("B")
  val nC = Nonterminal("C")
  val nD = Nonterminal("D")

  val testGrammar = Grammar(
    terminals = Set(ta, tc),
    nonterms = Set(nS, nA, nD),
    startSymbol = nS,
    productions = Set(
      nS -> Seq(nA, tc),
      nA -> Seq(nS, nD),
      nD -> Seq(ta, nD),
      nA -> Seq(ta)
    )
  )

  test("convert example grammar") {
    val expected = testGrammar.copy(
      productions = Set(
        nS -> Seq(nA, tc),
        nA -> Seq(ta)
      )
    )

    val tr = new UselessSymbolsEliminator()
    val actual = tr(testGrammar)

    actual shouldEqual expected
  }
}
