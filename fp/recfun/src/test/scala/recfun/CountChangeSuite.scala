package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  import Main.minCoins1
  import Main.minCoinChanges1
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF(300)") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies(301)") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF(300)") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }
  
  /*
  test("countChange: sorted CHF(900)") {
    assert(countChange(900,List(5,10,20,50,100,200,500)) === 65918)
  }
  
  test("countChange: no pennies(901)") {
    assert(countChange(901,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF(900)") {
    assert(countChange(900,List(500,5,50,100,20,200,10)) === 65918)
  }
  * 
  */

  /*
  test("countChange: sorted CHF(3000)") {
    assert(countChange(3000,List(5,10,20,50,100,200,500)) === 22481738)
  }
  * 
  */

  /*
  test("countChange: sorted CHF(30000)") {
    assert(countChange(30000,List(5,10,20,50,100,200,500)) === 443779442)
  }
  * 
  */
  
  test("minCoins1: unsorted CHF(300)") {
    assert(minCoins1(300,List(500,5,50,100,20,200,10)) === 2)
  }

  test("minCoins1: no pennies(301)") {
    assert(minCoins1(301,List(5,10,20,50,100,200,500)) === -1)
  }

  test("minCoins1: unsorted CHF(305)") {
    assert(minCoins1(305,List(500,5,50,100,20,200,10)) === 3)
  }
  
  test("minCoins1: unsorted CHF(365)") {
    assert(minCoins1(365,List(500,5,50,100,20,200,10)) === 5)
  }
  
  test("minCoins1: unsorted CHF(385)") {
    assert(minCoins1(385,List(500,5,50,100,20,200,10)) === 6)
  }

  test("minCoins1: unsorted CHF(885)") {
    assert(minCoins1(885,List(500,5,50,100,20,200,10)) === 7)
  }

  test("minCoinChanges1: unsorted CHF(850)") {
    assert(minCoinChanges1(850,List(500,5,50,100,20,200,10)) === 4)
  }

  //test("minCoinChanges1: unsorted CHF(3475)") {
  //  assert(minCoinChanges1(3475,List(500,5,50,100,20,200,10)) === 11)
  //}
}
