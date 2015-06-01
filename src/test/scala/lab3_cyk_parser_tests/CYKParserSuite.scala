package lab3_cyk_parser_tests

import org.scalatest.{Matchers, FunSuite}

import lab3_cyk.CYKParser

import common.{Grammar, Terminal, Nonterminal}
import common.Implicits.pair2production

import scala.util.{Success, Try}


class CYKParserSuite extends FunSuite with Matchers {
  val S = Nonterminal("S")
  val A = Nonterminal("A")

  val a_ = Terminal("a")
  val b = Terminal("b")

  val testGrammar = Grammar(
    terminals = Set(a_, b),
    nonterms = Set(A, S),
    startSymbol = S,
    productions = Set(
      S -> Seq(A, A),
      S -> Seq(A, S),
      S -> Seq(b),
      A -> Seq(S, A),
      A -> Seq(A, S),
      A -> Seq(a_)
    )
  )

  val cyk = new CYKParser(testGrammar)

  test("construct first row test") {
    val chain = Seq(a_, b, a_, a_, b)
    val expected: cyk.Table = Seq(
      Seq(Set(A), Set(S), Set(A), Set(A), Set(S))
    )
    val actual = cyk.constructFirstRow(chain)
    
    expected shouldEqual actual.get
  }

  test("production of {A, S} {A}") {
    val expected = Set(A, S)
    val actual = cyk.findProductions(Set(A, S), Set(A)).map(p => p.lhs)
    actual shouldEqual expected
  }

  test("production of {S} {A}") {
    cyk.findProductions(Set(S), Set(A)).map(_.lhs) shouldEqual Set(A)
  }

  test("construct table row") {
    val table = Seq(
      Seq(Set(A), Set(S), Set(A), Set(A), Set(S))
    )

    val expectedTable = Seq(
      Seq(Set(A),     Set(S),   Set(A),   Set(A),   Set(S)),
      Seq(Set(A, S),  Set(A),   Set(S),   Set(A, S))
    )

    val actual = cyk.constructRow(2, 5, table)

    actual shouldEqual expectedTable
  }

  test("construct table for chain 'abaab'") {
    val chain = Seq(a_, b, a_, a_, b)
    val expectedTable = Seq(
      Seq(Set(A),       Set(S),      Set(A),      Set(A),       Set(S)),
      Seq(Set(A, S),    Set(A),      Set(S),      Set(A, S)),
      Seq(Set(A, S),    Set(S),      Set(A, S)),
      Seq(Set(A, S),    Set(A, S)),
      Seq(Set(A, S))
    )

    val actualTable = cyk.constructTable(chain)

    actualTable shouldEqual Success(expectedTable)
  }

  test("validate chain 'abaab'") {
    val chain = Seq(a_, b, a_, a_, b)
    cyk.validate(chain) shouldBe Success(true)
  }

  test("left parse for 'abaab") {
    val chain = Seq(a_, b, a_, a_, b)
    val parse = cyk.leftParse(chain)

    assert(true)
  }
}
