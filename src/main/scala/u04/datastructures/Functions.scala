package u04.datastructures

// a module, including various functions and definitions
object Functions extends App:

  // a function (i.e. method), using recursion and case-match
  def factorial(n: Int): Int = n match
    case 0 | 1 => 1
    case _ => n * factorial(n - 1)

  println(factorial(5)) // 120

  // a function (i.e. method), using currying, and passing a function
  def applyManyTimes[A](initial: A, n: Int)(f: A => A): A = n match
    case 0 => initial
    case _ => applyManyTimes(f(initial), n - 1)(f)

  println(applyManyTimes(0, 10)(i => i + 2)) // 20
  println(applyManyTimes(0, 10)(_ + 2)) // equivalent formulation

  // a record data type
  case class Point2D(x: Double, y: Double)

  // using extension
  extension (p: Point2D) def multiply(d: Double): Point2D = p match
    case Point2D(x, y) => Point2D(d * x, d * y)

  println(multiply(Point2D(10, 20))(1.5))
  println(Point2D(10, 20).multiply(1.5))
  println(Point2D(10, 20) multiply 1.5)
