package grammar_transform_tests

import common.{EmptySymbol, Grammar, Terminal, Nonterminal}
import common.Implicits._
import grammar_transforms.EpsRulesEliminator
import org.scalatest.{Matchers, FunSuite}

object ExampleGrammarEps {
  val S = Nonterminal("S")
  val A = Nonterminal("A")
  val B = Nonterminal("B")
  val C = Nonterminal("C")

  val a = Terminal("a")
  val b = Terminal("b")
  val c = Terminal("c")
  val d = Terminal("d")

  val e = EmptySymbol

  val grammar = Grammar(
    terminals = Set(a, b, c, d),
    nonterms = Set(S, A, B, C),
    startSymbol = S,
    productions = Set(
      S   ->  Seq(A, B, C, d),
      A   ->  Seq(a),
      A   ->  Seq(e),
      B   ->  Seq(A, C),
      C   ->  Seq(c),
      C   ->  Seq(e)
    )
  )

  val expectedGrammar = grammar.copy(
    productions = Set(
      S -> Seq(A, d),
      S -> Seq(A, B, d),
      S -> Seq(A, C, d),
      S -> Seq(A, B, C, d),
      S -> Seq(B, d),
      S -> Seq(B, C, d),
      S -> Seq(C, d),
      S -> Seq(d),

      A -> Seq(a),

      B -> Seq(A),
      B -> Seq(A, C),
      B -> Seq(C),

      C -> Seq(c)
    )
  )
}

class EpsRulesEliminatorSuite extends FunSuite with Matchers {
  test("works for example grammar") {
    val tr = new EpsRulesEliminator()
    val actual = tr(ExampleGrammarEps.grammar)
    actual shouldEqual ExampleGrammarEps.expectedGrammar
  }
}
