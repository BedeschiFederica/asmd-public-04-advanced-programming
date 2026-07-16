package u04.typeclasses

object TypeClasses :

  trait Combine[T]:
    def combine(t1: T)(t2: T): T
    extension (t1:T) def <>(t2: T): T = combine(t1)(t2) // warning

  object SumInt extends Combine[Int]:
    def combine(t1: Int)(t2: Int): Int = t1 + t2

  object ProdInt extends Combine[Int]:
    def combine(t1: Int)(t2: Int): Int = t1 * t2
  
  object ConcatStrings extends Combine[String]:
    def combine(t1: String)(t2: String): String = t1 + t2
  
  def combine3[T](using cmb: Combine[T])(t1: T, t2: T, t3: T): T =
    t1 <> t2 <> t3

  def combine4[T: Combine](t1: T, t2: T, t3: T): T =
    t1 <> t2 <> t3

  @main def ex1 =
    println(combine3(using SumInt)(1,10,100))
    println(combine3(using ProdInt)(1,10,100))

    given Combine[Int] = ProdInt
    println(1 <> 2)
    println(combine3(1,10,100))
