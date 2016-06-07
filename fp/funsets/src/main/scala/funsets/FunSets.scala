package funsets

import scala.annotation.tailrec

//for ScalaCheck
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  def emptySet: Set =
    x => false
  
  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = 
    x => if (elem == x) true else false
  
  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = 
    x => if (contains(s, x) || contains(t, x)) true else false
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = 
    x => if (contains(s, x) && contains(t, x)) true else false
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = 
    x => if (contains(s, x) && !contains(t, x)) true else false
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = 
    x => if (contains(s, x) && p(x)) true else false
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    @tailrec
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false // within `s`
      else iter(a+1)
    }
    iter(-1 * bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = 
      !forall(s, x => !(p(x)))
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = 
      x => exists(s, y => f(y) == x)
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}

class QuickCheckFunSets extends Properties("FunSets.Set") {
  def associatSet(setOp: (FunSets.Set, FunSets.Set) => FunSets.Set,
        st: FunSets.Set, sv: FunSets.Set, fun: Int => Int, e: Int): Boolean = {
    val sa = setOp(FunSets.map(st, fun), FunSets.map(sv, fun)) 
    val sb = FunSets.map(setOp(st, sv), fun)
    !FunSets.diff(sa, sb)(e) && !FunSets.diff(sb, sa)(e)
  }
  val ints = Gen.choose(-1000, 1000)
  
  lazy val genSet1: Gen[FunSets.Set] = for {
    a <- ints
    //oneOf elements type should be Gen[FunSets.Set], const converts FunSets.Set to Gen[FunSets.Set]
    s <- oneOf(const(FunSets.emptySet), genSet1)
  } yield FunSets.union(FunSets.singletonSet(a), s)
  //FunSets function union is used here to generate arbitrary set
  //all following checks are based on this arbitrary set
  //at least functions union and singletonSet used here cannot be checked for correctness
  //other functions might also be affected.
  //functions should be separated into two categories, one for generating the arbitrary sets
  //which are used to check the other category correctness.
  //only second category functions can be checked, rather than the first category functions.
  //but if the first category functions are all basic functions from ScalaCheck itself, there should be no problem.
  //The problem is how to generate arbitrary elements for user-defined types only with basic ScalaCheck functions.
  lazy val genSet: Gen[FunSets.Set] = for {
    a <- ints // this makes generated genSet safe
    //oneOf elements type should be Gen[FunSets.Set], const converts FunSets.Set to Gen[FunSets.Set]
    s <- oneOf(const(FunSets.emptySet), genSet)
  } yield x => if (x == a || s(a)) true else false
  //now genSet is basic, it does not use any functions in FunSets, except emptySet
  
  lazy val genInt: Gen[Int] = for {
    a <- ints
  } yield a
  
  implicit lazy val arbSet: Arbitrary[FunSets.Set] = Arbitrary(genSet)
  
  implicit lazy val arbInt: Arbitrary[Int] = Arbitrary(genInt)
  
  //following descriptions are not displayed in ScalaTest? so there is no clue which property fails
  //== or === defined for FunSets.Set? No!
  // e should be ints, but it's OK, since no forall or exists called.

  property("contains, singletonSet") = forAll { (s: FunSets.Set, e: Int) =>
    FunSets.contains(FunSets.singletonSet(e), e) && !FunSets.contains(FunSets.diff(s, FunSets.singletonSet(e)), e)
  }
  
  property("union, intersect, and diff") = forAll { (s1: FunSets.Set, s2: FunSets.Set, e: Int) => {
      val s = FunSets.union(FunSets.diff(s1, s2), FunSets.intersect(s1, s2))
      !FunSets.diff(s, s1)(e) && !FunSets.diff(s1, s)(e)
    }
  }

  // lots of problems here, bound (1000) might be the problem. -reason: forall implementation is not correct
  //p: Int => Boolean, p is also a Set
  property("filter, forall, and exists") = forAll { (s: FunSets.Set, p: FunSets.Set) =>
    FunSets.forall(FunSets.filter(s, p), p) && !FunSets.exists(FunSets.diff(s, FunSets.filter(s, p)), p) 
  }

  //map check is tricky
  property("map") = forAll { (s1: FunSets.Set, s2: FunSets.Set, f: Int => Int, e: Int) => {//e might be out of bound
    associatSet(FunSets.union, s1, s2, f, e) &&
    //this is true for any x (in intersect) f(x) would not be dropped, since both sets hold it
    // even if f is not one-to-one mapping
    associatSet(FunSets.intersect, s1, s2, f, e) //&&
    //diff does not hold, only if f is one-to-one mapping, since for x in diff(s1, s2), f(x) might also be mapped from y
    // which is in s2, but not in s1, such that f(y) = f(x), f(y) is dropped by result diff, thus, f(x) is dropped
    //ScalaCheck tested this out
    //associatSet(FunSets.diff, s1, s2, f, e)
    }
  }
  
}
