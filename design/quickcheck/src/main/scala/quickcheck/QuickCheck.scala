package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  //TODO: implement this function
  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a,h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  //TODO: more properties, but the less the better, only if with all cases
  //ScalaCheck is better than ScalaTest, since it's functional thus can do functional programming
  //on testing such as the following composition
  //ScalaCheck and ScalaTest on the same project?
  property("del1 -- and unique property") = 
    forAll { (h1: H, h2: H) =>
      findMin(meld(h1, deleteMin(h2))) == findMin(insert(findMin(h1), deleteMin(meld(deleteMin(h1), h2))))
      // the following also works
      //findMin(deleteMin(meld(h1, h2))) == findMin(deleteMin(insert(findMin(h1), meld(deleteMin(h1), h2))))
  }  

}
