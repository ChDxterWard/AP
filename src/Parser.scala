case class Parser(rawInput: String) {
  val input = rawInput.filterNot(ele => ele.isWhitespace)
  val (vaTokenRe, calcTokenRe) = Tokenizer.tokenize(input.filterNot(ele => ele.isWhitespace), (Nil, Nil))
  val (varToken, calcToken) = (vaTokenRe.reverse, calcTokenRe.reverse)
  val vars = varToken.map(ele => {
    val arr = ele.varValue.split("=")
    val (name, wert) = (arr(0), arr(1))
    Var(name, wert.toDouble)
  })
  val states: scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map[String, Double]() ++ vars.map(variable => (variable.name -> variable.wert))


  def eval() = {
    val (operation, rest) = Expr(calcToken)
    if (Nil != rest) {
      println("Fehler")
    }
    Algebra(vars, operation)
  }


  // E->T( (+|-)T)*
  // E->T | (+|-)E
  // Expr -> Term RExpr
  def Expr(in: List[Token]): (Operation, List[Token]) = {
    val (term1, rest1) = Term(in)
    val (rexpr1, rest2) = Rexpr(term1, rest1)
    (rexpr1, rest2)
  }

  // Term -> Faktor RTerm
  def Term(in: List[Token]): (Operation, List[Token]) = {
    val (faktor1, rest1) = Faktor(in)
    val (rterm, rest2) = RTerm(faktor1, rest1)
    (rterm, rest2)
  }

  // RTerm -> * Faktor RTerm
  def RTerm(op1: Operation, in: List[Token]): (Operation, List[Token]) = {
    if (Nil == in) {
      (op1, in)
    }
    else {
      val firstChar1 = in(0).varValue
      val rest1 = in.slice(1, in.length)

      if (firstChar1 == "*" || firstChar1 == "/" || firstChar1 == "^") {
        val (faktor1, rest2) = Faktor(rest1)
        val op2 = if (firstChar1 == "*") Mul(op1, faktor1) else if (firstChar1 == "/") Div(op1, faktor1) else Pow(op1, faktor1)
        val (rterm, rest3) = RTerm(op2, rest2)
        (rterm, rest3)
      }
      else {
        (op1, in)
      }
    }
  }

  // Rexpr -> + Term Rexpr | e
  def Rexpr(op1: Operation, in: List[Token]): (Operation, List[Token]) = {
    if (Nil == in) {
      (op1, in)
    }
    else {
      val firstChar1 = in(0).varValue
      val rest1 = in.slice(1, in.length)

      if (firstChar1 == "+" || firstChar1 == "-") {
        val (term1, rest2) = Term(rest1)
        val op2: Operation = if (firstChar1 == "+") Add(op1, term1) else Sub(op1, term1)
        val (rexpr1, rest3) = Rexpr(op2, rest2)
        (rexpr1, rest3)
      }
      else {
        (op1, in)
      }
    }
  }

  // Faktor -> Num | ( Expr )
  def Faktor(in: List[Token]): (Operation, List[Token]) = {
    val h = in(0).varValue
    if (h == "(") {
      val rest1 = in.slice(1, in.length)
      val (expr1, rest2) = Expr(rest1)
      val h2 = rest2(0).varValue
      if (")" != h2) {
        println("\tFehler in Faktor")
      }
      val rest3 = rest2.slice(1, rest2.length)
      (Num(expr1.calc(states)), rest3)
    }
    else {
      try {
        (Num(h.toDouble), in.slice(1, in.length))
      } catch {
        //case _: NumberFormatException => (Num(states.get(h.varValue).get), in.slice(1, in.length))
        case _: NumberFormatException => (Str(h), in.slice(1, in.length))
      }
    }
  }
}
