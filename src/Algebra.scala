import scala.collection.mutable
abstract sealed class Operation {
  def calc(states: scala.collection.mutable.Map[String, Double]): Double
}

case class Var(val name: String, val wert: Double)  {}

case class Str(val name: String) extends Operation {
  override def calc(states: scala.collection.mutable.Map[String, Double]): Double = {
    states.get(name).get
  }
}

case class Num(val wert: Double) extends Operation {
  override def calc(states: scala.collection.mutable.Map[String, Double]): Double = wert
}

case class Add(val op1: Operation, val op2: Operation) extends Operation {
  override def calc(states: scala.collection.mutable.Map[String, Double]): Double = op1.calc(states) + op2.calc(states)
}

case class Sub(val op1: Operation, val op2: Operation) extends Operation {
  /*override def calc(states: scala.collection.mutable.Map[String, Double]): Double = {

    //println(getLuR(states).length)

    getLuR(states).foldLeft()

    //op1.calc(states) - op2.calc(states)
  }*/
  override def calc(states: mutable.Map[String, Double]): Double = {
    op2 match {
      case sub: Sub => {
        val sub1 = op1.calc(states)
        val sub2 = sub.op1.calc(states)
        val sub3 = sub.op2.calc(states)
        (sub1 - sub2) - sub3
      }
      case _ => {
        op1.calc(states) - op2.calc(states)
      }
    }
  }
  /*def getLuR(states: mutable.Map[String, Double]): List[Double] = {
    val lNums = op1 match {
      case sub: Sub => sub.getLuR(states)
      case _ => List(op1.calc(states))
    }
    val rNums = op2 match {
      case sub: Sub => sub.getLuR(states)
      case _ => List(op2.calc(states))
    }
    lNums ::: rNums
  }*/
}

case class Mul(val op1: Operation, val op2: Operation) extends Operation {
  override def calc(states: scala.collection.mutable.Map[String, Double]): Double = op1.calc(states) * op2.calc(states)
}

case class Div(val op1: Operation, val op2: Operation) extends Operation {
  override def calc(states: scala.collection.mutable.Map[String, Double]): Double = op1.calc(states) / op2.calc(states)
}
case class Pow(val op1: Operation, val op2: Operation) extends Operation {
  override def calc(states: scala.collection.mutable.Map[String, Double]): Double = Math.pow(op1.calc(states), op2.calc(states))
}

case class Algebra(val vars: List[Var], val op: Operation) {
  val states: scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map[String, Double]() ++ vars.map(variable => (variable.name -> variable.wert))
  def calc(): Double = op.calc(states)
}