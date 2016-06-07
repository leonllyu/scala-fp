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
  def literal: Gen[Literal] = for {
    double <- arbitrary[Double]
  } yield Literal(double)
    
  def ref: Gen[Ref] = for {
    s <- arbitrary[String]
    if (s.size != 0)
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
  
  def exprs: Gen[Expr] = Gen.oneOf(literal, ref, plus, minus, times, divide)
  
  implicit lazy val arbExpr: Arbitrary[Expr] = Arbitrary(exprs)

  def genExprMap(s1: String, s2: String, e1: Expr, e2: Expr): Map[String, Signal[Expr]] = {
    var ref: collection.mutable.Map[String, Signal[Expr]] = collection.mutable.Map[String, Signal[Expr]]()
    if (s1.size != 0 && s2.size != 0 && !s1.equals(s2)) {
      ref(s1) = Signal(e1)
      ref(s2) = Signal(e2)
    }
    ref.toMap //convert mutable Map to immutable Map
  }
  
  property("plus/minus/times/divide expr is automatically updated with signal") = forAll {
    (s1: String, s2: String, e1: Expr, e2: Expr) => {
      if (s1.size == 0 || s2.size == 0 || s1.equals(s2)) true
      else {
        genExprMap(s1, s2, e1, e2) == genExprMap(s1, s2, Plus(e1, e2), e2)
        genExprMap(s1, s2, e1, e2) == genExprMap(s1, s2, Minus(e1, e2), e2)
        genExprMap(s1, s2, e1, e2) == genExprMap(s1, s2, Times(e1, e2), e2)
        genExprMap(s1, s2, e1, e2) == genExprMap(s1, s2, Divide(e1, e2), e2)
  
        val oldValues = Calculator.computeValues(genExprMap(s1, s2, e1, e2))
        val newValues = Calculator.computeValues(genExprMap(s1, s2, Plus(e1, e2), e2))
        //.map(s => s == olds + e2)

        val references = genExprMap(s1, s2, e1, e2)
        var olde = e1
        val oldSignal = Signal(olde)
        olde = Plus(olde, e2)
        Calculator.eval(olde, references) - Calculator.eval(oldSignal(), references) < 0.0001 //compare doubles
        
        olde = Minus(olde, e2)
        Calculator.eval(olde, references) - Calculator.eval(oldSignal(), references) < 0.0001 //compare doubles
        
        olde = Times(olde, e2)
        Calculator.eval(olde, references) - Calculator.eval(oldSignal(), references) < 0.0001 //compare doubles   
        
        if (e2 == 0)
          olde = Divide(olde, Plus(e2, Literal(1.0)))
        else olde = Divide(olde, e2)
        Calculator.eval(olde, references) - Calculator.eval(oldSignal(), references) < 0.0001 //compare doubles
      }     
    }   
  }
}