package lab2_nonnullable

import common._
import common.Implicits.pair2production


class NonnullableConverter {
  def convert(g: Grammar): Grammar = {
    val nullables = findNullables(g)
    val reversed: Map[Nonterminal, Nonterminal] =
      nullables.map(n => (n, Nonterminal("!" + n.name))).toMap

    val newProductions = createNewProductions(g, nullables, reversed)

    val newStartSymbol =
      if (nullables.contains(g.startSymbol)) reversed(g.startSymbol)
      else g.startSymbol

    new Grammar(
      terminals = g.terminals,
      nonterms = g.nonterms ++ reversed.values,
      productions = newProductions,
      startSymbol = newStartSymbol
    )
  }

  def findNullables(g: Grammar): Set[Nonterminal] = {
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


  def createNewProductions(g: Grammar, nullables: Set[Nonterminal],
                           reversed: Map[Nonterminal, Nonterminal])
  : Set[Production] = {
    val newProductions = g.productions
      .filter(p => p.rhs != Seq(EmptySymbol))
      .flatMap { p =>
        val (bs, xs) = split(p.rhs, nullables)
        val m = bs.length
        val n = xs.length

        val prods: Seq[Production] =
          if (m > 0)
            (0 until m) map {i =>
              Production(p.lhs, (reversed(bs(i)) +: bs.takeRight(m - i - 1)) ++ xs)
            }
          else if (n > 0) Seq(p.lhs -> xs)
          else Seq()

        if (nullables contains p.lhs)
          prods ++ prods.map(p => Production(reversed(p.lhs), p.rhs))
        else prods
      }

    val emptyProductions = g.productions.filter(p => p.rhs == Seq(EmptySymbol))

    newProductions ++ emptyProductions
  }

  def split(chain: Seq[Symbol], nullables: Set[Nonterminal]): (Seq[Nonterminal], Seq[Symbol]) = {
    val bs: Seq[Nonterminal] = chain.takeWhile {
      case nonterm: Nonterminal => nullables.contains(nonterm)
      case _ => false
    } map {
      case nonterm: Nonterminal => nonterm
    }

    val xs = chain.drop(bs.length)

    (bs.toList, xs.toList)
  }
}
