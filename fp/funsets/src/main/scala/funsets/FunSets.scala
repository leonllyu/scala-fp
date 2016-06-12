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

class QuickCheckFunSets extends Properties("FunSets") {
import FunSets._
  def associatSet(setOp: (Set, Set) => Set,
        st: Set, sv: Set, fun: Int => Int, e: Int): Boolean = {
    val sa = setOp(FunSets.map(st, fun), FunSets.map(sv, fun)) 
    val sb = FunSets.map(setOp(st, sv), fun)
    !diff(sa, sb)(e) && !diff(sb, sa)(e)
  }
  val ints = Gen.choose(-1000, 1000)
  
  lazy val genSet1: Gen[Set] = for {
    a <- ints
    //oneOf elements type should be Gen[Set], const converts Set to Gen[Set]
    s <- oneOf(const(emptySet), genSet1)
  } yield union(singletonSet(a), s)
  //FunSets function union is used here to generate arbitrary set
  //all following checks are based on this arbitrary set
  //at least functions union and singletonSet used here cannot be checked for correctness
  //other functions might also be affected.
  //functions should be separated into two categories, one for generating the arbitrary sets
  //which are used to check the other category correctness.
  //only second category functions can be checked, rather than the first category functions.
  //but if the first category functions are all basic functions from ScalaCheck itself, there should be no problem.
  //The problem is how to generate arbitrary elements for user-defined types only with basic ScalaCheck functions.
  lazy val genSet: Gen[Set] = for {
    a <- ints // this makes generated genSet safe
    //oneOf elements type should be Gen[Set], const converts Set to Gen[Set]
    s <- oneOf(const(emptySet), genSet)
  } yield x => x == a || s(a)
  //now genSet is basic, it does not use any functions in FunSets, except emptySet
  
  implicit lazy val arbSet: Arbitrary[Set] = Arbitrary(genSet)
  
  implicit lazy val arbInt: Arbitrary[Int] = Arbitrary(ints)
  
  //following descriptions are not displayed in ScalaTest? so there is no clue which property fails
  //== or === defined for Set? No!
  property("contains, singletonSet") = forAll { (s: Set, e: Int) =>
    contains(singletonSet(e), e) && !contains(diff(s, singletonSet(e)), e)
  }
  
  property("union, intersect, and diff") = forAll { (s1: Set, s2: Set, e: Int) => {
      val s = union(diff(s1, s2), intersect(s1, s2))
      !diff(s, s1)(e) && !diff(s1, s)(e)
    }
  }

  // lots of problems here, the reason is that forall implementation is not correct, already corrected
  //p: Int => Boolean, p is also a Set
  property("filter, forall, and exists") = forAll { (s: Set, p: Set) =>
    forall(filter(s, p), p) && !exists(diff(s, filter(s, p)), p) 
  }

  //map check is tricky
  property("map") = forAll { (s1: Set, s2: Set, f: Int => Int, e: Int) => {
    associatSet(union, s1, s2, f, e) &&
    //this is true for any x (in intersect) f(x) would not be dropped, since both sets hold it
    // even if f is not one-to-one mapping
    associatSet(intersect, s1, s2, f, e) && //there are 5% failed cases, why?
    //diff does not hold, only if f is one-to-one mapping, since for x in diff(s1, s2), f(x) might also be mapped from y
    // which is in s2, but not in s1, such that f(y) = f(x), f(y) is dropped by result diff, thus, f(x) is dropped
    //ScalaCheck tested this out
    //associatSet(diff, s1, s2, f, e)
    true
    }
  }
  
}
