package grammar_transform_tests

import common.{EmptySymbol, Grammar, Nonterminal, Terminal}
import common.Implicits._
import grammar_transforms.UnitRulesEliminator
import org.scalatest.{Matchers, FunSuite}

class UnitRulesEliminatorSuite extends FunSuite with Matchers {
  val ta = Terminal("a")
  val tb = Terminal("b")
  val tc = Terminal("c")

  val eps = EmptySymbol

  val nS = Nonterminal("S")
  val nA = Nonterminal("A")
  val nB = Nonterminal("B")
  val nC = Nonterminal("C")
  val nD = Nonterminal("D")

  val exGrammar = Grammar(
    terminals = Set(ta, tb),
    nonterms = Set(nS, nA, nB, nC),
    startSymbol = nS,
    productions = Set(
      nS -> Seq(nA, nB, nC),
      nS -> Seq(nA, nB),
      nS -> Seq(nB, nC),
      nS -> Seq(nA),
      nS -> Seq(nB),
      nS -> Seq(nC),
      nS -> Seq(eps),

      nA -> Seq(nB, nB),
      nA -> Seq(nB),

      nB -> Seq(nC, nC),
      nB -> Seq(nC),
      nB -> Seq(ta),

      nC -> Seq(nA, nA),
      nC -> Seq(nA),
      nC -> Seq(tb)
    )
  )

  val ex2Grammar = Grammar(
    terminals = Set(ta, tb),
    nonterms = Set(nS, nA, nB, nC, nD),
    startSymbol = nS,
    productions = Set(
      nA -> Seq(nB),
      nA -> Seq(ta),
      nB -> Seq(nC),
      nB -> Seq(tb),
      nC -> Seq(nD, nD),
      nC -> Seq(tc)
    )
  )

  test("finds N_A for example grammar") {
    val tr = new UnitRulesEliminator()
    val actual = tr.findN(exGrammar, nA)
    actual shouldEqual Set(nS, nA, nB, nC)
  }

  test("finds N_B for example grammar") {
    val tr = new UnitRulesEliminator()
    val actual = tr.findN(exGrammar, nB)
    actual shouldEqual Set(nS, nA, nB, nC)
  }

  test("finds N_C for example grammar") {
    val tr = new UnitRulesEliminator()
    val actual = tr.findN(exGrammar, nC)
    actual shouldEqual Set(nS, nA, nB, nC)
  }

  test("finds N_A for example grammar 2") {
    val tr = new UnitRulesEliminator()
    val actual = tr.findN(ex2Grammar, nA)
    actual shouldEqual Set()
  }

  test("finds N_B for example grammar 2") {
    val tr = new UnitRulesEliminator()
    val actual = tr.findN(ex2Grammar, nB)
    actual shouldEqual Set(nA)
  }

  test("finds N_C for example grammar 2") {
    val tr = new UnitRulesEliminator()
    val actual = tr.findN(ex2Grammar, nC)
    actual shouldEqual Set(nB, nA)
  }

  test("convert example grammar 2") {
    val tr = new UnitRulesEliminator()
    val expected = ex2Grammar.copy(
      productions = Set(
        nA -> Seq(nD, nD), nA -> Seq(ta), nA -> Seq(tb), nA -> Seq(tc),
        nB -> Seq(nD, nD), nB -> Seq(tb), nB -> Seq(tc),
        nC -> Seq(nD, nD), nC -> Seq(tc)
      )
    )
    val actual = tr(ex2Grammar)
    actual shouldEqual expected
  }
}
