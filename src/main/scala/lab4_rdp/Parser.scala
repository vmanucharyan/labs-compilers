package lab4_rdp

import scala.util.{Failure, Success, Try}


class Parser(val inputChain: String, val cursor: Int) {
  val trimmedChain = inputChain.trim

  def parse(): Try[Boolean] =
    program().map(p => p.trimmedChain.isEmpty)


  def program(): Try[Parser] = block()


  def block(): Try[Parser] = {
    println("ENTERING BLOCK")
    consume("{").flatMap(_.operatorsList()).flatMap(_.consume("}"))
  }


  def operatorsList(): Try[Parser] = {
    println("ENTERING OPERATOR LIST")
    operator().flatMap(_.tail())
  }


  def operator(): Try[Parser] = {
    println("ENTERING OPERATOR")
    identifier().flatMap(_.consume("=")).flatMap(_.expression())
      .recover { case e: ParserException => this }
  }


  def tail(): Try[Parser] = {
    println("ENTERING TAIL")
    consume(";").flatMap(_.operator()).flatMap(_.tail())
      .recover { case e: ParserException => this }
  }


  def identifier(): Try[Parser] = {
    println("ENTERING IDENTIFIER")
    consumeWord() flatMap { case (symbol, parser) =>
      if (symbol.head.isLetter) Success(parser)
      else {
        println(s"$symbol is not identifier")
        Failure(ParserException("identifier starts with letter"))
      }
    }
  }


  def expression(): Try[Parser] = {
    println("ENTERING EXPRESSION")
    arithmeticExpression().flatMap(_.cmpOp()).flatMap(_.arithmeticExpression())
      .recoverWith { case _: ParserException => arithmeticExpression() }
  }


  def arithmeticExpression(): Try[Parser] = {
    println("ENTERING ARITHMETIC EXPRESSION")
    term().recoverWith { case _: ParserException =>
      term().flatMap(_.arithmeticExpression1())
    }
  }


  def arithmeticExpression1(): Try[Parser] = {
    println("ENTERING ARITHMETIC EXPRESSION1")
    addOp().flatMap(_.term())
      .recoverWith { case _: ParserException =>
        addOp().flatMap(_.term()).flatMap(_.arithmeticExpression1())
      }
  }


  def term(): Try[Parser] = {
    println("ENTERING TERM")

    factor().recoverWith { case _: ParserException =>
      factor().flatMap(_.term1())
    }
  }


  def term1(): Try[Parser] = {
    println("ENTERING TERM1")

    mulOp().flatMap(_.factor()).recoverWith { case _: ParserException =>
      mulOp().flatMap(_.factor()).flatMap(_.term1())
    }
  }


  def factor(): Try[Parser] = {
    println("ENTERING FACTOR1")

    identifier().recoverWith { case _: ParserException =>
      constant().recoverWith { case _: ParserException =>
        consume("(").flatMap(_.arithmeticExpression()).flatMap(_.consume(")"))
      }
    }
  }


  def constant(): Try[Parser] = {
    println("ENTERING CONSTANT")
    consumeNumber().map { case (_, p) => p }
  }


  def cmpOp(): Try[Parser] = {
    println("ENTERING CMP")

    consume("<=").recoverWith { case _: ParserException =>
      consume("<>").recoverWith { case _: ParserException =>
        consume("<").recoverWith { case _: ParserException =>
          consume(">=").recoverWith { case _: ParserException =>
            consume(">")
          }
        }
      }
    }
  }


  def addOp(): Try[Parser] = {
    consume("+").recoverWith { case _: ParserException => consume("-") }
  }


  def mulOp(): Try[Parser] = {
    consume("*").recoverWith { case _: ParserException => consume("/") }
  }


  def consume(symbol: String): Try[Parser] = {
    if (trimmedChain.startsWith(symbol)) {
      println(s"consumed $symbol from $trimmedChain")
      Success(new Parser(trimmedChain.drop(symbol.length), cursor + symbol.length))
    }
    else
      Failure(ParserException(s"($cursor) expected $symbol in $trimmedChain"))
  }


  def consumeWord(): Try[(String, Parser)] = {
    val symbol = trimmedChain.takeWhile(c => !Parser.separators.contains(c))

    println(s"consumed $symbol from $trimmedChain")

    if (symbol.nonEmpty) {
      Success(symbol -> new Parser(trimmedChain.drop(symbol.length), cursor + symbol.length))
    }
    else Failure(ParserException(s"expected identifier, got $trimmedChain"))
  }


  def consumeNumber(): Try[(Int, Parser)] = {
    val numberString = trimmedChain.takeWhile(c => Parser.digits.contains(c))
    if (numberString.isEmpty)
      Failure(ParserException(s"expected number in ${trimmedChain.take(10)}..."))
    else {
      println(s"consuming $numberString")
      Success(numberString.toInt ->
        new Parser(trimmedChain.drop(numberString.length), cursor + numberString.length))
    }
  }
}


object Parser {
  val digits = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  val separators = Set(' ', '\n', '+', '-', '<', '>', '=', '!', '*', '/', '(', ')', '{' , '}')
  def apply(str: String): Parser = new Parser(str, 0)
}

case class ParserException(msg: String) extends RuntimeException(msg)