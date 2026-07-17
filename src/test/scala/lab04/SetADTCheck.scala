package lab04

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Properties}
import lab04.SetADTs.{BasicSetADT, SetADT, TreeSetADT}

abstract class SetADTCheck(name: String) extends Properties(name):
  val setADT: SetADT
  import setADT.*

  // generating a small Int
  def smallInt(): Gen[Int] = Gen.choose(0, 10)

  // generating a Set of Int with approximate size (modulo clashes)
  def setGen[A: Arbitrary](size: Int): Gen[Set[A]] =
    if size == 0
      then Gen.const(empty())
    else for
      a <- Arbitrary.arbitrary[A]
      s <- setGen(size - 1)
    yield s.add(a)

  // a given instance to generate sets with small size
  given arb: Arbitrary[Set[Int]] = Arbitrary:
    for
      i <- smallInt()
      s <- setGen[Int](i)
    yield s

  /**
   * axioms defining contains based on empty/add:
   * contains(empty, x) = false
   * contains(add(x,s), y) = (x == y) || contains(s, y)
   */
  property("axioms for contains") =
    forAll: (s: Set[Int], x: Int, y:Int) =>
      s.add(x).contains(y) == (x == y) || s.contains(y)
    &&
    forAll: (x: Int) =>
      !empty().contains(x)

  /**
   * axioms defining remove based on empty/add:
   * - remove(x, empty) = empty
   * - remove(x, add(x, s)) = remove(x, s)
   * - remove(x, add(y, s)) = add(y, remove(x, s)) if x!=y
   */
  property("axioms for remove") =
    forAll: (x: Int) =>
      empty().remove(x) === empty()
    &&
    forAll: (s: Set[Int], x: Int) =>
      s.add(x).remove(x) === s.remove(x)
    &&
    forAll: (s: Set[Int], x: Int, y: Int) =>
      (x != y) ==> (s.add(y).remove(x) === s.remove(x).add(y))

  /**
   * axioms defining size based on add/remove:
   * - size(add(x, s)) = if contains(s, x) then size(s) else size(s) + 1
   * - size(remove(x, s)) = if contains(s, x) then size(s) - 1 else size(s)
   */
  property("axioms for size") =
    forAll: (s: Set[Int], x: Int) =>
      s.add(x).size() == (if s.contains(x) then s.size() else s.size() + 1)
    &&
    forAll: (s: Set[Int], x: Int) =>
      s.remove(x).size() == (if s.contains(x) then s.size() - 1 else s.size())

  /** common algebraic properties:
   * - commutativity: operation(s1, s2) = operation(s2, s1)
   * - associativity: operation(operation(s1, s2), s3) = operation(s1, operation(s2, s3))
   * - idempotence: operation(s, s) = s
   */

  enum AlgebraicProperty:
    case Commutativity
    case Associativity
    case Idempotence

  import AlgebraicProperty.*

  private def checkAlgebraicProperty(algebraicProperty: AlgebraicProperty, operationName: String,
                                     operation: (Set[Int], Set[Int]) => Set[Int]): Unit = algebraicProperty match
    case Commutativity =>
      property(s"commutativity of $operationName") =
        forAll: (s1: Set[Int], s2: Set[Int]) =>
          operation(s1, s2) === operation(s2, s1)
    case Associativity =>
      property(s"associativity of $operationName") =
        forAll: (s1: Set[Int], s2: Set[Int], s3: Set[Int]) =>
          operation(operation(s1, s2), s3) === operation(s1, operation(s2, s3))
    case Idempotence =>
      property(s"idempotence of $operationName") =
        forAll: (s: Set[Int]) =>
          operation(s, s) === s

  /** algebraic properties for union:
   * - commutativity: union(s1, s2) = union(s2, s1)
   * - associativity: union(union(s1, s2), s3) = union(s1, union(s2, s3))
   * - idempotence: union(s, s) = s
   * - the empty set is the identity element: union(s, empty) = s
   */
  Seq(Commutativity, Associativity, Idempotence).foreach(checkAlgebraicProperty(_, "union", _ || _))
  property("the empty set is the identity element for union") =
    forAll: (s: Set[Int]) =>
      (s || empty()) === s

  /** algebraic properties for intersection:
   * - commutativity: intersection(s1, s2) = intersection(s2, s1)
   * - associativity: intersection(intersection(s1, s2), s3) = intersection(s1, intersection(s2, s3))
   * - idempotence: intersection(s, s) = s
   * - the empty set is the absorbing element: intersection(s, empty) = empty
   */
  Seq(Commutativity, Associativity, Idempotence).foreach(checkAlgebraicProperty(_, "intersection", _ && _))
  property("the empty set is the absorbing element for intersection") =
    forAll: (s: Set[Int]) =>
      (s && empty()) === empty()

  /** cross-properties between union and intersection:
   * - distributivity of intersection over union:
   *     intersection(s1, union(s2, s3)) = union(intersection(s1, s2), intersection(s1, s3)
   * - distributivity of union over intersection:
   *     union(s1, intersection(s2, s3)) = intersection(union(s1, s2), union(s1, s3)
   * - absorption laws:
   *     union(s1, intersection(s1, s2)) = s1
   *     intersection(s1, union(s1, s2)) = s1
   */

  property("distributivity of intersection over union") =
    forAll: (s1: Set[Int], s2: Set[Int], s3: Set[Int]) =>
      (s1 && (s2 || s3)) === ((s1 && s2) || (s1 && s3))

  property("distributivity of union over intersection") =
    forAll: (s1: Set[Int], s2: Set[Int], s3: Set[Int]) =>
      (s1 || (s2 && s3)) === ((s1 || s2) && (s1 || s3))

  property("absorption laws") =
    forAll: (s1: Set[Int], s2: Set[Int]) =>
      (s1 || (s1 && s2)) === s1
    &&
    forAll: (s1: Set[Int], s2: Set[Int]) =>
      (s1 && (s1 || s2)) === s1


object BasicSetADTCheck extends SetADTCheck("SequenceBased Set"):
  val setADT: SetADT = BasicSetADT

  @main def visualizingCheckArbitrarySets(): Unit =
    Range(0,20).foreach(i => println(summon[Arbitrary[setADT.Set[Int]]].arbitrary.sample))


object TreeSetADTCheck extends SetADTCheck("TreeBased Set"):
  val setADT: SetADT = TreeSetADT
