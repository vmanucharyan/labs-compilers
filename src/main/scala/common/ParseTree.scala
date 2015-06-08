package common

class ParseTree {

}

trait ParseTreeNode {
  def symbol: Symbol
}

case class ParseTreeTerminal(symbol: Terminal) extends ParseTreeNode
case class ParseTreeNonterminal(symbol: Nonterminal) extends ParseTreeNode