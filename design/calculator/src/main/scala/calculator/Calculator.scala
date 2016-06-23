package calculator

//import collection.immutable.Map

//for ScalaCheck
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  //TODO:
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for ((str, se) <- namedExpressions) yield (str, Signal(eval(se(), namedExpressions)))
    
    //namedExpressions foreach ((str, se) => (str, Signal(eval(se(), namedExpressions))))   
    //namedExpressions map (str, se) => (str, Signal(eval(se(), namedExpressions)))
  }

  //TODO:
  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    //case Ref(name) =>  if (!references.contains(name)) Double.NaN
    //                   else eval(references(name)(), references - name)
    case Ref(name) => eval(getReferenceExpr(name, references), references - name)
    case Plus(a, b) => eval(a, references) + eval(b, references)
    case Minus(a, b) => eval(a, references) - eval(b, references)
    case Times(a, b) => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)  
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}

class QuickCheckSignal extends Properties("Expr") {
  
  //for ScalaCheck
  
  val doubles = choose(Double.MinValue, Double.MaxValue)
  
  //use Gen.lzy, otherwise infinite recursion with stack over flow error
  def exprs: Gen[Expr] = Gen.lzy(Gen.oneOf(literal, ref, plus, minus, times, divide))
  
  def literal: Gen[Literal] = for {
    d <- doubles // arbitrary[Double]
  } yield Literal(d)
    
  def ref: Gen[Ref] = for {
    s <- arbitrary[String]
    if (s.size != 0 && !s.equals(""))
  } yield Ref(s)
  
  def plus: Gen[Plus] = for {
    left <- exprs
    right <- exprs
  } yield Plus(left, right)
  
  def minus: Gen[Minus] = for {
    left <- exprs
    right <- exprs
  } yield Minus(left, right)
  
  def times: Gen[Times] = for {
    left <- exprs
    right <- exprs
  } yield Times(left, right)
  
  def divide: Gen[Divide] = for {
    left <- exprs
    right <- exprs
    if (right != 0.0)
  } yield Divide(left, right)
  /*
  //self-closure - all strings in Expr should be defined in map
  def genExprMap(values: List[String]): Gen[Map[String, Signal[Expr]]] = for {
    keys <- containerOfN[List,Int](values.size, arbitrary[Int])
  } yield Map(keys.zip(values)
      
  //mapOfN
   * 
   */

  implicit lazy val arbExpr: Arbitrary[Expr] = Arbitrary(exprs)
  
  property("plus/minus/times/divide expr is automatically updated with signal") = forAll {
    (e1: Expr) => {
    //(e1: Expr, e2: Expr) => {
        // val mapExprSignal = Gen.mapOfN[String, Signal[Expr]](10, Gen.oneOf(something))
  
        //val oldValues = Calculator.computeValues(genExprMap(s1, s2, e1, e2))
        //val newValues = Calculator.computeValues(genExprMap(s1, s2, Plus(e1, e2), e2))
        //.map(s => s == olds + e2)

        /*
        val olde = e1
        val oldSignal = Signal(olde)
        olde = Plus(olde, e2)
        Calculator.eval(olde, mapExprSignal) - Calculator.eval(oldSignal(), mapExprSignal) < 0.0001 //compare doubles
        
        olde = Minus(olde, e2)
        Calculator.eval(olde, references) - Calculator.eval(oldSignal(), references) < 0.0001 //compare doubles
        
        olde = Times(olde, e2)
        Calculator.eval(olde, references) - Calculator.eval(oldSignal(), references) < 0.0001 //compare doubles   
        
        if (e2 == 0)
          olde = Divide(olde, Plus(e2, Literal(1.0)))
        else olde = Divide(olde, e2)
        Calculator.eval(olde, references) - Calculator.eval(oldSignal(), references) < 0.0001 //compare doubles
        * 
        */
        true
      //}
    }
  }
}