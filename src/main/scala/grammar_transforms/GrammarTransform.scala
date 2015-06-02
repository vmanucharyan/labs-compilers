package grammar_transforms

import common.Grammar

trait GrammarTransform {
  def apply(grammar: Grammar): Grammar
}
