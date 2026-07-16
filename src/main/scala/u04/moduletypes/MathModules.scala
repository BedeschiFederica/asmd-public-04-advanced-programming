package u04.moduletypes

object MathModules :
  
  // type of a module
  trait MathModuleType:
    def factorial(n: Int): Int
    def exp(base: Double, power: Int): Double

  // non-tail recursive solutions without input checks
  object BasicMathModule extends MathModuleType:

    override def factorial(n: Int): Int = 
      if n == 0 then 1 else n * factorial(n - 1)

    override def exp(base: Double, power: Int): Double = 
      if power == 0 then 1 else base * exp(base, power - 1)

  // tail recursive solutions with input checks
  object ProductionMathModule extends MathModuleType:

    override def factorial(n: Int): Int = 
      @scala.annotation.tailrec
      def _fact(n: Int, temp: Int): Int =
        if n == 0 then temp else _fact(n - 1, temp * n)
      n match
        case _ if n >= 0 => _fact(n, 1)

    override def exp(base: Double, power: Int): Double = 
      @scala.annotation.tailrec
      def _exp(p: Int, temp: Double): Double =
        if p == 0 then temp else _exp(p - 1, temp * base)
      power match   
        case _ if base >= 0 => _exp(power, 1)

@main def tryMathModule =
  import MathModules.*

  // probability of having x successes over n trials, where each success has prob. p
  // abstracting from specific implementation of the math module
  def binomialProbability(mm: MathModuleType)(n: Int, x: Int, p: Double): Double =
    mm.factorial(n) / mm.factorial(x) / mm.factorial(n-x) * 
    mm.exp(p, x) * mm.exp(1-p, n-x)

  // probability of 6 heads on 10 coin tosses
  println:
    binomialProbability(BasicMathModule)(10, 6, 0.5)
  println:
    binomialProbability(ProductionMathModule)(10, 6, 0.5)
  
