package grammar_transforms

import common.{Production, Nonterminal, Grammar}

class UnitRulesEliminator extends GrammarTransform {
  override def apply(grammar: Grammar): Grammar = {
    val unitProducers: Set[(Nonterminal, Set[Nonterminal])] =
      grammar.nonterms.map(n => (n, findN(grammar, n)))

    val newRules: Set[Production] = unitProducers.flatMap {
      case (nt, ntN) =>
        val pairs = ntN.map(s => (s, nt))
        pairs.flatMap {
          case (ntA, ntB) =>
            val nonUnitB = grammar.productions
              .filterNot(isUnit)
              .filter(p => p.lhs == ntB)
            nonUnitB.map(p => Production(ntA, p.rhs))
        }
    }

    grammar.copy(
      productions = grammar.productions.filterNot(isUnit) ++ newRules
    )
  }

  def isUnit(p: Production): Boolean = {
    p match {
      case Production(_, Seq(Nonterminal(_))) => true
      case _ => false
    }
  }

  def findN(grammar: Grammar, nA: Nonterminal): Set[Nonterminal] = {
    def recur(pN: Set[Nonterminal]): Set[Nonterminal] = {
      val iN = findNi(grammar, pN)
      val newN = iN ++ pN

      if (newN == pN) pN
      else recur(newN)
    }

    val fN = findN0(grammar, nA)

    recur(fN)
  }

  def findN0(grammar: Grammar, nA: Nonterminal): Set[Nonterminal] = {
    grammar.nonterms.filter { nB =>
      grammar.productions.contains(Production(from = nB, to = Seq(nA)))
    }
  }

  def findNi(grammar: Grammar, pN: Set[Nonterminal]): Set[Nonterminal] = {
    grammar.nonterms.filter { ntC =>
      grammar.productions.exists { p =>
        p.lhs == ntC && p.rhs.forall(ntD => pN.exists(s => s == ntD))
      }
    }
  }
}
