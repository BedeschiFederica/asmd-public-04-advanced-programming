package scala.lab04

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties, Test}

import scala.lab04.Sequences.*
import scala.lab04.Sequences.Sequence.*


object SequenceCheck extends Properties("Sequence"):

  override def overrideParameters(parameters: Test.Parameters): Test.Parameters =
    parameters
      .withMinSuccessfulTests(200)  // default: 100
      //.withInitialSeed(12345)

  // define a recursive generator of lists, monadically
  def sequenceGen[A: Arbitrary](): Gen[Sequence[A]] = for
    i <- arbitrary[A]
    b <- Gen.prob(0.8)
    s <- if b then sequenceGen().map(s2 => Cons(i, s2)) else Gen.const(Nil())
  yield s

  // define custom arbitrary lists and mappers
  given intSeqArbitrary: Arbitrary[Sequence[Int]] = Arbitrary(sequenceGen[Int]())
  given mapperArbitrary: Arbitrary[Int => Int] = Arbitrary(Gen.oneOf[Int => Int]( _+1, _*2, x => x*x))

  // check axioms, universally
  property("mapAxioms") =
    forAll: (seq: Sequence[Int], f: Int => Int) =>
      //println(seq); println(f(10)) // inspect what's using
      (seq, f) match
        case (Nil(), f) =>  map(Nil())(f) == Nil()
        case (Cons(h, t), f) => map(Cons(h, t))(f) == Cons(f(h), map(t)(f))

  property("sumAxioms") =
    forAll: (seq: Sequence[Int]) =>
      seq match
        case Nil() => Nil().sum == 0
        case Cons(h, t) => Cons(h, t).sum == h + t.sum

  property("filterAxioms") =
    forAll: (seq: Sequence[Int], pred: Int => Boolean) =>
      //println(seq); println(pred(10))
      (seq, pred) match
        case (Nil(), pred) => Nil().filter(pred) == Nil()
        case (Cons(h, t), pred) =>
          Cons(h, t).filter(pred) == (if pred(h) then Cons(h, t.filter(pred)) else t.filter(pred))

  property("flatMapAxioms") =
    forAll: (seq: Sequence[Int], f: Int => Sequence[Int]) =>
      //println(seq); println("-> " + f(10))
      (seq, f) match
        case (Nil(), f) => Nil().flatMap(f) == Nil()
        case (Cons(h, t), f) => Cons(h, t).flatMap(f) == f(h).concat(t.flatMap(f))

  // how to check a generator works as expected
  @main def showSequences() =
    Range(0,20).foreach(i => println(summon[Arbitrary[Sequence[Int]]].arbitrary.sample))
