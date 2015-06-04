package grammar_transforms

import common.Grammar

class ChomskyConverter extends GrammarTransform {
  val chain = GrammarTransformChain(Seq(
    new LongRulesEliminator(),
    new EpsRulesEliminator(),
    new UnitRulesEliminator(),
    new UselessSymbolsEliminator(),
    new OneTerminalConverter()
  ))

  override def apply(grammar: Grammar): Grammar = chain.apply(grammar)
}