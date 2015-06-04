package grammar_transforms

import common.{EmptySymbol, Production, Nonterminal, Grammar}
import common.Implicits.pair2production
import common.SetExt._

class EpsRulesEliminator extends GrammarTransform {
  override def apply(grammar: Grammar): Grammar = {
    val epsProducers = findEpsProducers(grammar)
    val newProd: Set[Production] = grammar.productions.flatMap { prod =>
      val prodEpsProducers = prod.rhs
        .filter(symbol => epsProducers.exists(ep => ep == symbol))

      val combinations = prodEpsProducers.toSet.combinations()
      val newProductions = combinations
        .map(combination => Production(prod.lhs, prod.rhs.filter(s => !combination.contains(s))))
      newProductions + prod
    }

    val newGrammar =
      grammar.copy(productions = newProd.filter(p => p.rhs != Seq(EmptySymbol) && p.rhs != Seq()))

    if (epsProducers.contains(grammar.startSymbol)) {
      val oldStart = newGrammar.startSymbol
      val newStart = Nonterminal("EPS_ELIM_" + newGrammar.startSymbol.name)
      newGrammar.copy(
        startSymbol = newStart,
        nonterms = newGrammar.nonterms + newStart,
        productions = newGrammar.productions ++ Set(
          Production(newStart, Seq(oldStart)),
          Production(newStart, Seq(EmptySymbol))
        )
      )
    }
    else newGrammar
  }

  def findEpsProducers(g: Grammar): Set[Nonterminal] = {
    def step(nullables: Set[Production]): Set[Production] = {
      val nullableSymbols = nullables.map(p => p.from)
      val newProductions = g.productions
        .diff(nullables)
        .filter(p => p.to.forall {
          case s: Nonterminal => nullableSymbols.contains(s)
          case _ => false
        })

      if (newProductions.isEmpty) nullables
      else step(nullables ++ newProductions)
    }

    val oneStepNullables = g.productions.filter(p => p.to == Seq(EmptySymbol))
    val nullProductions = step(oneStepNullables)

    nullProductions.map(p => p.from)
  }
}
