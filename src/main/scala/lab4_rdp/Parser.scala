package lab4_rdp

import java.util.function.BinaryOperator

import scala.util.{Failure, Success, Try}


case class Parser(inputChain: String, cursor: Int, values: Map[String, Double]) {
  import Parser.BinaryOperation

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
    this
      .identifier()
      .flatMap{ case (idName, p) => p.consume("=").map(p1 => (idName, p1)) }
      .flatMap{ case (idName, p) =>
          p.expression().map { case (exprRes, p1: Parser) =>
            p1.copy(values = values + (idName -> exprRes) )
          }
      }
      .recover { case e: ParserException => this }
  }


  def tail(): Try[Parser] = {
    println("ENTERING TAIL")
    consume(";").flatMap(_.operator()).flatMap(_.tail())
      .recover { case e: ParserException => this }
  }


  def identifier(): Try[(String, Parser)] = {
    println("ENTERING IDENTIFIER")
    consumeWord() flatMap { case (symbol, parser) =>
      if (symbol.head.isLetter) Success(symbol -> parser)
      else {
        println(s"$symbol is not identifier")
        Failure(ParserException("identifier starts with letter"))
      }
    }
  }


  def expression(): Try[(Double, Parser)] = {
    println("ENTERING EXPRESSION")
    this
      .arithmeticExpression()
      .flatMap { case (value, p) => p.cmpOp().map{ case (op, p1) => (value, op, p1)} }
      .flatMap { case (value1: Double, op: BinaryOperation, p: Parser) =>
        p.arithmeticExpression().map { case (value2, p1) => (op(value1, value2), p1)}
      }
      .recoverWith { case _: ParserException => arithmeticExpression() }
  }


  def arithmeticExpression(): Try[(Double, Parser)] = {
    println("ENTERING ARITHMETIC EXPRESSION")
    term().flatMap{ case (value, p) => p.arithmeticExpression1(value) }
      .recoverWith { case _: ParserException => term() }
  }


  def arithmeticExpression1(prefix: Double): Try[(Double, Parser)] = {
    println("ENTERING ARITHMETIC EXPRESSION1")

    this
      .addOp()
      .flatMap { case (op, p) => p.term().map { case (termVal, p1) => (op(prefix, termVal), p1) } }
      .flatMap { case (value, p) => p.arithmeticExpression1(prefix) }
      .recoverWith { case _: ParserException =>
        this
          .addOp()
          .flatMap { case (op, p) =>
            p.term().map { case (termRes, p1) => (op(prefix, termRes), p1) }
          }
      }
  }


  def term(): Try[(Double, Parser)] = {
    println("ENTERING TERM")
    factor().flatMap { case (value, p) => p.term1(value) }
      .recoverWith { case _: ParserException => factor() }
  }


  def term1(prefix: Double): Try[(Double, Parser)] = {
    println("ENTERING TERM1")

    this
      .mulOp()
      .flatMap{ case (op, mulp) =>
        mulp.factor().map { case (facres, facp) => op(prefix, facres) -> facp }
      }
      .flatMap { case (value, p) => p.term1(value) }
      .recoverWith { case _: ParserException =>
        mulOp().flatMap { case (op, p) =>
          p.factor().map { case (value, p1) => op(prefix, value) -> p1 }
        }
      }
  }


  def factor(): Try[(Double, Parser)] = {
    println("ENTERING FACTOR1")

    this
      .consume("(")
      .flatMap(_.arithmeticExpression())
      .flatMap{ case (value, p) => p.consume(")").map(consp => (value, consp)) }
      .recoverWith { case _: ParserException =>
        identifier().map { case (name, p) => (values(name), p)  }
          .recoverWith { case _: ParserException =>
            constant()
          }
      }
  }

  def constant(): Try[(Double, Parser)] = {
    println("ENTERING CONSTANT")
    consumeNumber()
  }


  def cmpOp(): Try[(BinaryOperation, Parser)] = {
    println("ENTERING CMP")

    val le = (v1: Double, v2: Double) => if (v1 <= v2) 1.0 else 0.0
    val l = (v1: Double, v2: Double) => if (v1 < v2) 1.0 else 0.0
    val eq = (v1: Double, v2: Double) => if (v1 == v2) 1.0 else 0.0
    val be = (v1: Double, v2: Double) => if (v1 >= v2) 1.0 else 0.0
    val b = (v1: Double, v2: Double) => if (v1 > v2) 1.0 else 0.0

    consume("<=").map(p => (le, p)).recoverWith { case _: ParserException =>
      consume("<>").map(p => (eq, p)).recoverWith { case _: ParserException =>
        consume("<").map(p => (l, p)).recoverWith { case _: ParserException =>
          consume(">=").map(p => (be, p)).recoverWith { case _: ParserException =>
            consume(">").map(p => (b, p))
          }
        }
      }
    }
  }


  def addOp(): Try[(BinaryOperation, Parser)] = {
    consume("+").map(p => ((v1: Double, v2: Double) => v1 + v2, p) )
      .recoverWith { case _: ParserException =>
        consume("-").map(p => ((v1: Double, v2: Double) => v1 - v2, p) )
    }
  }


  def mulOp(): Try[(BinaryOperation, Parser)] = {
    consume("*")
      .map(p => ( (v1: Double, v2: Double) => v1 * v2 , p) )
      .recoverWith {
        case _: ParserException =>
          consume("/").map(p => ( (v1: Double, v2: Double) => v1 / v2, p) )
      }
  }


  def consume(symbol: String): Try[Parser] = {
    if (trimmedChain.startsWith(symbol)) {
      println(s"consumed $symbol from $trimmedChain")
      Success(new Parser(trimmedChain.drop(symbol.length), cursor + symbol.length, values))
    }
    else
      Failure(ParserException(s"($cursor) expected $symbol in $trimmedChain"))
  }


  def consumeWord(): Try[(String, Parser)] = {
    val symbol = trimmedChain.takeWhile(c => !Parser.separators.contains(c))

    println(s"consumed $symbol from $trimmedChain")

    if (symbol.nonEmpty) {
      Success(symbol -> new Parser(trimmedChain.drop(symbol.length), cursor + symbol.length, values))
    }
    else Failure(ParserException(s"expected identifier, got $trimmedChain"))
  }


  def consumeNumber(): Try[(Double, Parser)] = {
    val numberString = trimmedChain.takeWhile(c => Parser.digits.contains(c))
    if (numberString.isEmpty)
      Failure(ParserException(s"expected number in ${trimmedChain.take(10)}..."))
    else {
      println(s"consuming $numberString")
      Success(numberString.toDouble ->
        new Parser(trimmedChain.drop(numberString.length), cursor + numberString.length, values))
    }
  }
}


object Parser {
  type BinaryOperation = (Double, Double) => Double

  val digits = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  val separators = Set(' ', '\n', '+', '-', '<', '>', '=', '!', '*', '/', '(', ')', '{' , '}')

  def apply(str: String): Parser = new Parser(str, 0, Map())
}

case class ParserException(msg: String) extends RuntimeException(msg)