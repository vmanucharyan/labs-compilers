package lab3_cyk

import common._
import common.SetExt._

import scala.util.Try

/**
 * Cocke–Younger–Kasami algorithm
 */
class CYKParser(val grammar: Grammar) {
  type Table = Seq[Seq[Set[Nonterminal]]]
  type Chain = Seq[Terminal]

  def validate(chain: Chain): Try[Boolean] = {
    constructTable(chain)
      .map(table => table.last.head.contains(grammar.startSymbol))
  }

  def constructTable(chain: Chain): Try[Table] = {
    constructFirstRow(chain).map { initialTable =>
      val n = initialTable.head.length
      (2 to n).foldLeft(initialTable)((acc, j) => constructRow(j, n, acc))
    }
  }

  def constructFirstRow(chain: Chain): Try[Table] = Try {
    val row: Seq[Set[Nonterminal]] = chain map { s =>
      grammar.productions
        .filter(p => p.rhs == Seq(s))
        .map(p => p.lhs)
    }

    if (row.contains(Set.empty)) throw new IllegalArgumentException("invalid input chain")
    else Seq(row)
  }

  def constructRow(j: Int, n: Int, table: Table): Table = {
    val row: Seq[Set[Nonterminal]] = for (i <- 1 to n - j + 1) yield {
      val setA: Set[Nonterminal] =
        (1 until j).flatMap(k => table(k - 1)(i - 1)).toSet

      val setB: Set[Nonterminal] =
        (1 until j).flatMap(k => table(j - k - 1)(i + k - 1)).toSet

      findProductions(setA, setB).map(p => p.lhs)
    }

    table :+ row
  }

  def findProductions(a: Set[Nonterminal], b: Set[Nonterminal]): Set[Production] = {
    val combinations = a cartesianProduct b
    combinations flatMap {
      case (s1, s2) => grammar.productions.filter(p => p.rhs == Seq(s1, s2))
    }
  }
}
