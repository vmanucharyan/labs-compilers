package common

class Tokenizer(val terminalSet: Set[Terminal]) {
  val terminalsMap = terminalSet.map(t => (t.name, t)).toMap

  def apply(str: String): Seq[Terminal] =
    str.split(' ').map(token => terminalsMap(token))
}
