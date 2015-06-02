package grammar_transform_tests

import common.{Terminal, Grammar, Nonterminal}
import common.Implicits._
import grammar_transforms.LongRulesEliminator
import org.scalatest.{Matchers, FunSuite}

object ExampleGrammar {
  val S = Nonterminal("S")
  val A = Nonterminal("A")
  val B = Nonterminal("B")

  val a = Terminal("a")
  val b = Terminal("b")
  val c = Terminal("c")
  val d = Terminal("d")
  val e = Terminal("e")
  val f = Terminal("f")

  val grammar = Grammar(
    terminals = Set(a, b, c),
    nonterms = Set(S, A, B),
    startSymbol = S,
    productions = Set(
      S   ->  Seq(A, B),
      A   ->  Seq(a, B, c, B),
      B   ->  Seq(d, e, f)
    )
  )

  val A1 = Nonterminal("A1")
  val A2 = Nonterminal("A2")
  val B1 = Nonterminal("B1")

  val expectedGrammar = Grammar(
    terminals = grammar.terminals,
    nonterms = Set(S, A, B, A1, A2, B1),
    startSymbol = S,
    productions = Set(
      S   ->  Seq(A, B),
      A   ->  Seq(a, A1),
      A1  ->  Seq(B, A2),
      A2  ->  Seq(c, B),
      B   ->  Seq(d, B1),
      B1  ->  Seq(e, f)
    )
  )
}

class LongRulesEliminatorSuite extends FunSuite with Matchers {
  test("works with example grammar") {
    val el = new LongRulesEliminator()
    val actual = el(ExampleGrammar.grammar)
    actual shouldEqual ExampleGrammar.expectedGrammar
  }
}
