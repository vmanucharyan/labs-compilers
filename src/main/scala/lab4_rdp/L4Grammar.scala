package lab4_rdp

import common.{Grammar, Nonterminal, Terminal}
import common.Implicits._

trait L4Grammar {
  val plus = Terminal("+")
  val minus = Terminal("-")
  val mul = Terminal("*")
  val div = Terminal("/")

  val less = Terminal("<")
  val big = Terminal(">")
  val le = Terminal("<=")
  val be = Terminal(">=")

  val id = Terminal("id")
  val const = Terminal("const")

  val openBrace = Terminal("(")
  val closeBrace = Terminal(")")

  val plusOp = Nonterminal("PLUS")
  val mulOp = Nonterminal("MUL")
  val cmpOp = Nonterminal("CMP")

  val factor = Nonterminal("FACTOR")
  val term = Nonterminal("TERM")
  val aExpr = Nonterminal("AEXPR")
  val expr = Nonterminal("EXPR")

  val l4Grammar = Grammar(
    terminals = Set(plus, minus, mul, div, less, big, le, be, id, const, openBrace, closeBrace),
    nonterms = Set(factor, term, aExpr, expr),
    startSymbol = expr,
    productions = Set(
      expr -> Seq(aExpr, cmpOp, aExpr),
      expr -> Seq(aExpr),

      aExpr -> Seq(aExpr, plusOp, term),
      aExpr -> Seq(term),

      term -> Seq(term, mulOp, factor),
      term -> Seq(factor),

      factor -> Seq(id),
      factor -> Seq(const),
      factor -> Seq(openBrace, aExpr, closeBrace),

      cmpOp -> Seq(less),
      cmpOp -> Seq(big),
      cmpOp -> Seq(le),
      cmpOp -> Seq(be),

      plusOp -> Seq(plus),
      plusOp -> Seq(minus),

      mulOp -> Seq(mul),
      mulOp -> Seq(div)
    )
  )
}
