package grammar_transforms

import common.{Nonterminal, Terminal, Production, Grammar}

class OneTerminalConverter extends GrammarTransform {
  override def apply(grammar: Grammar): Grammar = {
    val withTwoTerminals = grammar.productions filter {
      case Production(_, Seq(u1: Terminal, u2: Terminal)) => true
      case Production(_, Seq(u1, u2)) => false
      case Production(_, Seq(u)) => false
      case _ => throw new IllegalArgumentException("grammar contains long rules")
    }

    val newProductions: Set[Production] = withTwoTerminals.flatMap {
      case Production(lhs, Seq(tu1: Terminal, tu2: Terminal)) =>
        val nU1 = Nonterminal("ONE_TERM_" + tu1.name.toUpperCase)
        val nU2 = Nonterminal("ONE_TERM_" + tu2.name.toUpperCase)

        Set(
          Production(nU1, Seq(tu1)),
          Production(nU2, Seq(tu2)),
          Production(lhs, Seq(nU1, tu1)),
          Production(lhs, Seq(nU2, tu2))
        )
    }

    grammar.copy(
      nonterms = grammar.nonterms ++ newProductions.map(p => p.lhs),
      productions = grammar.productions.diff(withTwoTerminals) ++ newProductions
    )
  }
}
