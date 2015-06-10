package lab4_rdp

import scala.util.Try

object ExampleRDP {
  case class Context(rest: List[Char], consumed: Int) {
    def consume(char: Char): Try[Context] = Try {
      println(s"consuming $char in ${rest.mkString}")

      if (rest == Nil) throw new IllegalStateException(s"end of string")
      if (rest.head != char) throw new IllegalStateException(s"expected '$char' at $consumed")
      else Context(rest.tail, consumed + 1)
    }

    def head = rest.head
  }
}

/**
 * S -> (B)B
 * B -> (B)B
 * B -> empty
 */
class ExampleRDP {
  import ExampleRDP._

  def parse(str: String): Try[Boolean] = {
    S(Context(str.toList, 0))
      .map(_.rest.isEmpty)
      .recover { case _ => false }
  }

  def S(ctx: Context): Try[Context] = {
    ctx
      .consume('(')
      .flatMap(B)
      .flatMap(_.consume(')'))
      .flatMap(B)
  }

  def B(ctx: Context): Try[Context] = {
    ctx
      .consume('(')
      .flatMap(B)
      .flatMap(_.consume(')'))
      .flatMap(B)
      .recover {
        case e: IllegalStateException =>
          println("failed to consume '('. falling back")
          ctx
      }
  }
}
