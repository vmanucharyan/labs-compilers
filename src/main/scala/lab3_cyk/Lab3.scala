package lab3_cyk

import common.{Grammar, Terminal, Nonterminal}
import common.Implicits._


object Lab3 {
  val S = Nonterminal("S")
  val A = Nonterminal("A")

  val a = Terminal("a")
  val b = Terminal("b")

  val testGrammar = Grammar(
    terminals = Set(a, b),
    nonterms = Set(A, S),
    startSymbol = S,
    productions = Set(
      S -> Seq(A, A),
      S -> Seq(A, S),
      S -> Seq(b),
      A -> Seq(S, A),
      A -> Seq(A, S),
      A -> Seq(a)
    )
  )

  val cyk = new CYKParser(testGrammar)

  def main(args: Array[String]): Unit = {
    val chain = Seq(a, b, a, a, b)
    val parse = cyk.leftParse(chain)
    println(s"left parse: $parse")
  }
}
