package lab4_rdp

import common.{EmptySymbol, Nonterminal, Terminal, Grammar}
import common.Implicits._
import grammar_transforms.EpsRulesEliminator

import scala.util.{Failure, Success}

object Lab4 {
  def main(args: Array[String]): Unit = {
    val S = Nonterminal("S")
    val B = Nonterminal("B")
    val lb = Terminal("(")
    val rb = Terminal(")")
    val empty = EmptySymbol

    val grammar = Grammar(
      nonterms = Set(S, B),
      terminals = Set(lb, rb),
      startSymbol = S,
      productions = Set(
        S -> Seq(lb, B, rb, B),
        B -> Seq(lb, B, rb, B),
        B -> Seq(empty)
      )
    )

    val el = new EpsRulesEliminator()
    println(el(grammar))

    val ex = new ExampleRDP()
    println(ex.parse("((()(())))"))

    val inputString =
      """
        |{
        |    a = 12 > 5;
        |    b = 1 < 2;
        |    c = 51 < 1;
        |}
      """.stripMargin

    val parser = Parser(inputString)
    parser.parse() match {
      case Success(isValid) => println("Input string is valid")
      case Failure(e) => println("Failed to parse input string: " + e.getMessage);
    }
  }
}
