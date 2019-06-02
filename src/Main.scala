

object Main {
  def main(args: Array[String]): Unit = {

    println(Parser("3*1-1*2").eval().calc() == 1)
    println(Parser("x=3;3-x-3").eval().calc() == -3)
    println(Parser("3*1-1*2").eval().calc() == 1)
    println(Parser("y = 4;2* y -1").eval().calc() == 7)
    println(Parser("y = 4;y/2+1").eval().calc() == 3)
    println(Parser("y = 4;1+y/2").eval().calc() == 3)
    println(Parser("(1+1)").eval().calc() == 2)
    println(Parser("x=3;(1+x)*2-1").eval().calc() == 7)
    println(Parser("x=3;(x-x)-x").eval().calc() == -3)

    println(Parser("x=3;3-(3-3)").eval().calc() == 3)

    println(Parser("x=3;3-(3-3)").eval().calc() == 3)
    println(Parser("x=3;2^x+1").eval().calc() == 9)
    println(Parser("x=3;x^x-1").eval().calc() == 26)
    println(Parser("x=2;x^(x-1-x)").eval().calc() == .5)
    println(Parser("x=2;x^3/2").eval().calc() == 4)
    println(Parser("x=2;w=3;2*x - 3*w").eval().calc() == -5)
    println(Parser("x=2;w=3;2*(x - 3)*w").eval().calc() == -6)
    println(Parser("x=2;w=3;(2*(x - 3)*w)^2").eval().calc() == 36)
    println(Parser("x=2;w=3;((2*(x - 3)*w)^2)-1").eval().calc() == 35)
    println(Parser("x=2;w=3;(((2*(x - 3)*w)^2)-1)/7").eval().calc() == 5)
    println(Parser("x=2;w=3;((((2*(x - 3)*w)^2)-1)/7)^2-1").eval().calc() == 24)
    println(Parser("x=2;y=3;3*(x+y)/2").eval().calc() == 7.5)
    println(Parser("x=-2;y=3;(x-2+3)/2").eval().calc() == -.5)
    println(Parser("abc=1;32-abc").eval().calc() == 31)
    println(Parser("3*111-1*2").eval().calc() == 331)
    println(Parser("-3-1").eval().calc() == -4)
    println(Parser("(3*111-1*2)/2").eval().calc() == 165.5)
    println(Parser("(((3*111-1*2))/(2))").eval().calc() == 165.5)
    println(Parser("1-.5").eval().calc() == .5)
    println(Parser("-3*(2+1)").eval().calc() == -9)
  }
}





























