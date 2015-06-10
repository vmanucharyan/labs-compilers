package lab3_cyk

import common.{Tokenizer, Grammar, Terminal, Nonterminal}
import common.Implicits._
import grammar_transforms.{ChomskyConverter, LongRulesEliminator}




object Lab3 {
  val S = Nonterminal("S")
  val A = Nonterminal("A")

  val a = Terminal("a")
  val b = Terminal("b")

  val testGrammar = Grammar(
    terminals = Set(a, b),
    nonterms = Set(A, S),
    startSymbol = S,
    productions = Set(
      S -> Seq(A, A),
      S -> Seq(A, S),
      S -> Seq(b),
      A -> Seq(S, A),
      A -> Seq(A, S),
      A -> Seq(a)
    )
  )

  val cykTest = new CYKParser(testGrammar)


  val plus = Terminal("+")
  val minus = Terminal("-")
  val mul = Terminal("*")
  val div = Terminal("/")

  val less = Terminal("<")
  val big = Terminal(">")
  val le = Terminal("<=")
  val be = Terminal(">=")

  val id = Terminal("id")
  val const = Terminal("const")

  val openBrace = Terminal("(")
  val closeBrace = Terminal(")")

  val plusOp = Nonterminal("PLUS")
  val mulOp = Nonterminal("MUL")
  val cmpOp = Nonterminal("CMP")

  val factor = Nonterminal("FACTOR")
  val term = Nonterminal("TERM")
  val aExpr = Nonterminal("AEXPR")
  val expr = Nonterminal("EXPR")

  val l3Grammar = Grammar(
    terminals = Set(plus, minus, mul, div, less, big, le, be, id, const, openBrace, closeBrace),
    nonterms = Set(factor, term, aExpr, expr),
    startSymbol = expr,
    productions = Set(
      expr -> Seq(aExpr, cmpOp, aExpr),
      expr -> Seq(aExpr),

      aExpr -> Seq(aExpr, plusOp, term),
      aExpr -> Seq(term),

      term -> Seq(term, mulOp, factor),
      term -> Seq(factor),

      factor -> Seq(id),
      factor -> Seq(const),
      factor -> Seq(openBrace, aExpr, closeBrace),

      cmpOp -> Seq(less),
      cmpOp -> Seq(big),
      cmpOp -> Seq(le),
      cmpOp -> Seq(be),

      plusOp -> Seq(plus),
      plusOp -> Seq(minus),

      mulOp -> Seq(mul),
      mulOp -> Seq(div)
    )
  )


  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("usage: lab3 <input chain>")
      return
    }

    val chomskyConverter = new ChomskyConverter()
    val l3chomsky = chomskyConverter(l3Grammar).createLabels()
    val l3parser = new CYKParser(l3chomsky)

    val tokenizer = new Tokenizer(l3Grammar.terminals)
    val l3chain = tokenizer.apply(args(0))

    println("transformed grammar:")
    println(l3chomsky)
    println()

    println(args(0))
    println(s"chain: $l3chain")

    println("\nparse table:")
    val table = l3parser.constructTable(l3chain)
    table.get.foreach { row =>
      row.foreach(symbols => print(s"$symbols "))
      println()
    }
    println()

    val valid = l3parser.validate(l3chain)
    val leftparse = l3parser.leftParse(l3chain)

    println("left parse:")
    leftparse.get.zipWithIndex.foreach { case (p, i) => println(s"$i) $p") }

    println("left parse:")
    leftparse.get.foreach(p => print(s"${l3chomsky.labels(p)} "))
    println()

    println(s"$valid")
  }
}
