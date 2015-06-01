package common

import play.api.libs.json._

object JsonCodecs {
  implicit val terminalWrites = new Writes[Terminal] {
    def writes(terminal: Terminal) = Json.obj(
      "name" -> terminal.name,
      "type" -> "terminal"
    )
  }

  implicit val nonterminalWrites = new Writes[Nonterminal] {
    def writes(nonterminal: Nonterminal) = Json.obj(
      "name" -> nonterminal.name,
      "type" -> "nonterminal"
    )
  }

//  implicit val nonterminalReads = new Reads[Nonterminal] {
//    override def reads(json: JsValue): JsResult[Nonterminal] = {
//      val name = json \
//    }
//  }
}
