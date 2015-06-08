package common

case class Production(from: Nonterminal, to: Seq[Symbol]) {
  def rhs = to
  def lhs = from

  override def toString = {
    val symbolSeq = rhs.map(s => s.toString).mkString(" ")
    s"$lhs -> $symbolSeq"
  }
}

case class Grammar(terminals: Set[Terminal],
                   nonterms: Set[Nonterminal],
                   productions: Set[Production],
                   startSymbol: Nonterminal,
                   labels: Map[Production, Int] = Map()) {
  def createLabels() = Grammar.withLabels(
    terminals = terminals,
    nonterms = nonterms,
    productions = productions.toSeq
      .sortBy(p => if (p.lhs == startSymbol) -1 else 1)
      .zipWithIndex.map(p => (p._2, p._1)),
    startSymbol = startSymbol
  )

  override def toString: String = {
    if (labels.isEmpty) productions.map(p => p.toString).mkString("\n")
    else labels
      .toSeq
      .sortBy { case (prod, label) => label }
      .map { case (prod, label) => s"$label) $prod" }
      .mkString("\n")
  }
}

object Grammar {
  def withLabels(terminals: Set[Terminal],
                 nonterms: Set[Nonterminal],
                 productions: Seq[(Int, Production)],
                 startSymbol: Nonterminal): Grammar = {
    val labels = productions
      .map { case (label, prod) => (prod, label) }
      .toMap

    Grammar(terminals, nonterms, productions.map(_._2).toSet, startSymbol, labels)
  }
}

trait Symbol {
  def name: String
  override def toString = s"$name"
  def isTerminal = this.isInstanceOf[Terminal]
  def isNonterminal = this.isInstanceOf[Nonterminal]
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