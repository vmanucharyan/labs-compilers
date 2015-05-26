package common

case class Production(from: Nonterminal, to: Seq[Symbol]) {
  def rhs = to
  def lhs = from
}

case class Grammar(terminals: Set[Terminal],
                   nonterms: Set[Nonterminal],
                   productions: Set[Production],
                   startSymbol: Nonterminal)

trait Symbol {
  def name: String
}

case class Terminal(name: String) extends Symbol

case class Nonterminal(name: String) extends Symbol

object EmptySymbol extends Symbol {
  def name = "eps"
}

object Implicits {
  implicit def pair2production(p: (Nonterminal, Seq[Symbol])): Production = p match {
    case (s, seq) => Production(s, seq)
  }
}