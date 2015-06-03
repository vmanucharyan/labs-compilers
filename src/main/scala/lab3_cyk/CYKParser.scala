package lab3_cyk

import common._
import common.SetExt._
import common.Implicits.pair2production

import scala.collection.mutable.ListBuffer

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

  def leftParse(chain: Chain): Try[Seq[Production]] = {
    def gen(i: Int, j: Int, s: Nonterminal, table: Table, acc: ListBuffer[Production]): Unit = {
      if (j == 1) {
        if (grammar.productions.contains(s -> Seq(chain(i - 1))))
          acc.append(s -> Seq(chain(i - 1)))
      } else {
        val newProd =
          (1 until j).map { k =>
            val setB = table(k - 1)(i - 1)
            val setC = table(j - k - 1)(i + k - 1)
            val bcProductions: Set[Production] = findProductions(setB, setC)

            (k, bcProductions.find(p => p.lhs == s))
          }
          .filter { case (_, p) => p.isDefined }
          .map { case (k, p) => (k, p.get) }
          .head

        val (k, prod) = newProd

        acc.append(prod)

        gen(i, k, prod.rhs(0).asInstanceOf[Nonterminal], table, acc)
        gen(i + k, j - k, prod.rhs(1).asInstanceOf[Nonterminal], table, acc)
      }
    }

    validate(chain).flatMap { isValid =>
      constructTable(chain).map { table =>
        val acc = new ListBuffer[Production]
        gen(1, chain.length, grammar.startSymbol, table, acc)
        acc.toList
      }
    }
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
      val pairs: Seq[(Set[Nonterminal], Set[Nonterminal])] =
        for (k <- 1 until j) yield (table(k - 1)(i - 1), table(j - k - 1)(i + k - 1))

      val productions: Seq[Set[Production]] = pairs map {
        case (setA, setB) => findProductions(setA, setB)
      }

      productions.toSet.flatten.map(p => p.lhs)
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
