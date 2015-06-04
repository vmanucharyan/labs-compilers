package grammar_transforms

import common.Grammar

case class GrammarTransformChain(chain: Seq[GrammarTransform]) extends GrammarTransform {
  override def apply(grammar: Grammar): Grammar = {
    chain.foldLeft(grammar)((grammar, tr) => tr(grammar))
  }
}
