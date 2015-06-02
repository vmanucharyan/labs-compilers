package grammar_transforms

import common.{Production, Nonterminal, Grammar}

class LongRulesEliminator extends GrammarTransform {
  override def apply(grammar: Grammar): Grammar = {
    grammar.productions
      .filter(p => p.rhs.length > 2)
      .map { p =>
        val k = p.rhs.length

        val newSymbols =
          (0 until k - 2).map { i =>
            (p.rhs(i), Nonterminal(p.lhs.name + s"${i + 1}"))
          }

        val lastProduction = Production(
          from = newSymbols.last._2,
          to = Seq(p.rhs(k - 2), p.rhs(k - 1))
        )

        val newProductions = (k - 3 until 0 by -1).foldRight(Set(lastProduction))((i, acc) => {
          val (_, b_prev) = newSymbols(i - 1)
          val (a_cur, b_cur) = newSymbols(i)
          acc + Production(b_prev, Seq(a_cur, b_cur))
        })

        val firstProduction = Production(p.lhs, Seq(newSymbols.head._1, newSymbols.head._2))

        (p, newSymbols.map(_._2).toSet, newProductions + firstProduction)
      }
      .foldLeft(grammar)((acc, repl) => {
        repl match {
          case (prod, newNonterms, newProductions) =>
            println(s"deleted $prod, added $newProductions")
            acc.copy(
              nonterms = acc.nonterms ++ newNonterms,
              productions = (acc.productions - prod) ++ newProductions
            )
        }
      })
  }
}
