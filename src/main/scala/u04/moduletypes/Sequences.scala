package u04.moduletypes

object Sequences:
  
  trait SequenceADT:
    type Sequence[A]
    def cons[A](a: A, s: Sequence[A]): Sequence[A]
    def nil[A](): Sequence[A]
    def map[A, B](s1: Sequence[A], f: A => B): Sequence[B]
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A]

  object BasicSequenceADT extends SequenceADT:
    private enum SequenceImpl[A]:
      case Cons(a: A, t: Sequence[A])
      case Nil()
    import SequenceImpl.*

    opaque type Sequence[A] = SequenceImpl[A]

    override def cons[A](a: A, s: Sequence[A]) = Cons(a, s)
    override def nil[A](): Sequence[A] = Nil()

    override def concat[A](s1: Sequence[A], s2: Sequence[A]) = s1 match
      case Cons(a, s) => Cons(a, concat(s, s2))
      case _ => s2

    override def map[A, B](s1: Sequence[A], f: A => B) = s1 match
      case Cons(a, s) => Cons(f(a), map(s, f))
      case _ => Nil()

  object ScalaListSequenceADT extends SequenceADT:
    opaque type Sequence[A] = List[A]

    override def cons[A](a: A, s: Sequence[A]) = a :: s
    override def nil[A](): Sequence[A] = List()
    override def concat[A](s1: Sequence[A], s2: Sequence[A]) = s1 ++ s2
    override def map[A, B](s1: Sequence[A], f: A => B): Sequence[B] = s1.map(f)

@main def trySequencesADTModule =
  import Sequences.*
  val basicSequenceADT: SequenceADT = BasicSequenceADT
  val scalaListSequenceADT: SequenceADT = ScalaListSequenceADT

  {
    import basicSequenceADT.*
    val s1: Sequence[Int] = cons(10, cons(20, cons(30, nil())))
    println(concat(s1, s1)) // (10, 20, 30, 10, 20, 30)
    println(map(s1, _ >= 20)) // (false, true, true)
  }
  {
    import scalaListSequenceADT.*
    val s2: Sequence[Int] = cons(10, cons(20, cons(30, nil())))
    println(concat(s2, s2)) // (10, 20, 30, 10, 20, 30)
    println(map(s2, _ >= 20)) // (20, 30)
  }

  def sequenceOps(sADT: SequenceADT): Unit =
    import sADT.*
    val s1: Sequence[Int] = cons(10, cons(20, cons(30, nil())))
    println(concat(s1, s1)) // (10, 20, 30, 10, 20, 30)
    println(map(s1, _ >= 20)) // (false, true, true)

  sequenceOps(BasicSequenceADT)
  sequenceOps(ScalaListSequenceADT)