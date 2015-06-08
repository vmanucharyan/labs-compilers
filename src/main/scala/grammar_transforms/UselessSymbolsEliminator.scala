package grammar_transforms

import common.{Symbol, Terminal, Nonterminal, Grammar}

class UselessSymbolsEliminator extends GrammarTransform {
  override def apply(grammar: Grammar): Grammar = {
    def findUseful(useful: Set[Nonterminal]): Set[Nonterminal] = {
      val newUseful = grammar.productions
        .filter(p => p.rhs.exists {
          case nt: Nonterminal => useful.contains(nt)
          case _ => false
        })
        .map(p => p.lhs)

      val union = newUseful ++ useful

      //println(union)

      if (union == useful) useful
      else findUseful(newUseful ++ useful)
    }

    def isUseless(symbol: Symbol, usefulSymbols: Set[Nonterminal]): Boolean = symbol match {
      case nt: Nonterminal => !usefulSymbols.contains(nt)
      case _ => false
    }

    val clearlyUseful = grammar.productions
      .filter(p => p.rhs.forall(s => !s.isNonterminal))
      .map(p => p.lhs)

    //println(clearlyUseful)

    val useful =  findUseful(clearlyUseful)

    grammar.copy(
      productions = grammar.productions.filter { p =>
        !isUseless(p.lhs, useful) && !p.rhs.exists(s => isUseless(s, useful))
      }
    )
  }
}
