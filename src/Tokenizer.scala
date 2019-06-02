case class Token(varValue: String) {}
case object Tokenizer {
  def tokenize(in: String, tokens: (List[Token], List[Token]), firstTurn: Boolean = true):  (List[Token], List[Token]) = {
    val retVars = "^[A-Za-z]+=-?\\.?[0-9]+;".r.findFirstIn(in)
    val retChar = "^[A-Za-z]+".r.findFirstIn(in)
    val retOpenPatternMinus = "^-\\(".r.findFirstIn(in)
    val retOpen = "^\\(".r.findFirstIn(in)
    val retClose = "^\\)".r.findFirstIn(in)
    val retNumBegin = "^-?[0-9]*\\.?[0-9]+".r.findFirstIn(in)
    val retNum = "^[0-9]*\\.?[0-9]+".r.findFirstIn(in)
    val retMul = "^\\*".r.findFirstIn(in)
    val retSub = "^\\-".r.findFirstIn(in)
    val retDiv = "^\\/".r.findFirstIn(in)
    val retPow = "^\\^".r.findFirstIn(in)
    val retPlus = "^\\+".r.findFirstIn(in)
    if (firstTurn == true && retNumBegin != None) {
      tokenize(in.slice(retNumBegin.get.toString.length, in.length), (tokens._1, Token(retNumBegin.get.toString) :: tokens._2), false)
    }
    else if (retOpenPatternMinus != None) {
      val ret1 = in.slice(1, in.length)
      val ret2 = ret1.slice(1, ret1.length)
      //val t = t1 ::: Token(t2.toString) :: tokens._2
      //val t = List(Token("-"), Token("1"), Token("*"), Token("(")) ::: tokens._2
      val t = List(Token("("), Token("*"), Token("1"), Token("-")) ::: tokens._2
      tokenize(ret2, (tokens._1, t))
    }
    else if (retOpen != None)
      tokenize(in.slice(1, in.length), (tokens._1, Token(retOpen.get.toString) :: tokens._2), false)
    else if (retClose != None)
      tokenize(in.slice(1, in.length), (tokens._1, Token(retClose.get.toString) :: tokens._2), false)
    else if (retNum != None)
      tokenize(in.slice(retNum.get.length, in.length), (tokens._1, Token(retNum.get.toString) :: tokens._2), false)
    else if (retPlus != None)
      tokenize(in.slice(1, in.length), (tokens._1, Token(retPlus.get.toString) :: tokens._2), false)
    else if (retMul != None)
      tokenize(in.slice(1, in.length), (tokens._1, Token(retMul.get.toString) :: tokens._2), false)
    else if (retSub != None)
      tokenize(in.slice(1, in.length), (tokens._1, Token(retSub.get.toString) :: tokens._2), false)
    else if (retDiv != None)
      tokenize(in.slice(1, in.length), (tokens._1, Token(retDiv.get.toString) :: tokens._2), false)
    else if (retPow != None)
      tokenize(in.slice(1, in.length), (tokens._1, Token(retPow.get.toString) :: tokens._2), false)
    else if (retVars != None) {
      val token = Token(retVars.get.toString.slice(0, retVars.get.toString.length - 1))
      tokenize(in.slice(retVars.get.length, in.length), ( token :: tokens._1, tokens._2), false)
    }
    else if (retChar != None)
      tokenize(in.slice(retChar.get.length, in.length), (tokens._1, Token(retChar.get.toString) :: tokens._2), false)
    else
      (tokens._1, tokens._2)
  }
}