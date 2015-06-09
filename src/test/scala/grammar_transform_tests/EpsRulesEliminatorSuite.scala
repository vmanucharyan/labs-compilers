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

  val grammar2 = grammar.copy(
    productions = Set(
      S -> Seq(e), S -> Seq(A, b), S -> Seq(S, S),
      A -> Seq(a, S)
    )
  )

  val eS = Nonterminal("EPS_ELIM_S")
  val grammar2Expected = grammar2.copy(
    nonterms = grammar2.nonterms + eS,
    startSymbol = eS,
    productions = Set(
      eS -> Seq(e), eS -> Seq(S),
      S -> Seq(A, b), S -> Seq(S, S), S -> Seq(S),
      A -> Seq(a, S), A -> Seq(a)
    )
  )
}

class EpsRulesEliminatorSuite extends FunSuite with Matchers {
  test("works for example grammar") {
    val tr = new EpsRulesEliminator()
    val actual = tr(ExampleGrammarEps.grammar)
    actual shouldEqual ExampleGrammarEps.expectedGrammar
  }

  test("works with grammar where S ->* eps exists") {
    val tr = new EpsRulesEliminator()
    val actual = tr(ExampleGrammarEps.grammar2)
    actual shouldEqual ExampleGrammarEps.grammar2Expected
  }
}
