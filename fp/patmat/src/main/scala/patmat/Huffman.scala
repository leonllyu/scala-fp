package patmat

import scala.annotation.tailrec

//for ScalaCheck
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  

  // Part 1: Basics
  //TODO:
  def weight(tree: CodeTree): Int = tree match {
    case Leaf(c, w) => w
    case Fork(l, r, c, w) => weight(l) + weight(r)
  }
  
  //TODO:
  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(c, w) => List(c) //c::string2Chars("")
    case Fork(l, r, c, w) => c //chars(l) ::: chars(r)
  }
  
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  //TODO:
  def times(chars: List[Char]): List[(Char, Int)] = 
      //chars.groupBy(l => l).map(t => (t._1, t._2.length)).toList
      chars.groupBy(identity).mapValues(_.size).toList
  
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  //TODO:
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = 
      freqs.map(pair => new Leaf(pair._1, pair._2)).sortWith(weight(_) <= weight(_))
      //freqs.map(pair => new Leaf(pair._1, pair._2)).sortWith((leaf1, leaf2) => weight(leaf1) <= weight(leaf2))
  
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  //TODO:
  def singleton(trees: List[CodeTree]): Boolean = 
      if (trees.isEmpty || !trees.tail.isEmpty) false
      else true
  
  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  //TODO:
  def combine(trees: List[CodeTree]): List[CodeTree] = 
      if (trees.isEmpty || singleton(trees)) trees
      else (makeCodeTree(trees.head, trees.tail.head)::trees.tail.tail).sortWith(weight(_) <= weight(_))

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  //TODO:
  def until(xxx: List[CodeTree] => Boolean, yyy: List[CodeTree] =>List[CodeTree])(zzz: List[CodeTree]): CodeTree = {
      val newtrees: List[CodeTree] = yyy(zzz)
      if(xxx(newtrees)) newtrees.head
      else until(xxx, yyy)(newtrees)
  }
  
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  //TODO: generate Haffman tree based on the char usage frequencies 
  def createCodeTree(chars: List[Char]): CodeTree = 
      until(singleton, combine)(makeOrderedLeafList(times(chars)))
      
  //overloading does not work here, scala is same to java with type erasure on generics
  // after erasure both functions have the same signature createCodeTree(List)
  def createCodeTreeOnFreq(freq: List[(Char, Int)]): CodeTree =
      until(singleton, combine)(makeOrderedLeafList(freq))
  
  val englishCode: CodeTree = //including space?
    createCodeTreeOnFreq(List(('a', 8167), ('b', 1492), ('c', 2782), ('d', 4253), ('e', 12702), ('f', 2228),
               ('g', 2015), ('h', 6094), ('i', 6966), ('j', 153), ('k', 772), ('l', 4025), ('m', 2406),
               ('n', 6749), ('o', 7507), ('p', 1929), ('q', 95), ('r', 5987), ('s', 6327), ('t', 9056),
               ('u', 2758), ('v', 978), ('w', 2361), ('x', 150), ('y', 1974), ('z', 74)))
               
  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  //TODO: from list of bits to text based on Haffman tree, CodeTree here
  //@tailrec - not tailrec
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
          @tailrec
          def decodetravesal(tree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = tree match {
            case Fork(left, right, char, w) =>
              //if (bits.isEmpty) else('', Nil) //decode failed
              if (bits.head == 0) decodetravesal(left, bits.tail)
              else decodetravesal(right, bits.tail)
            case Leaf(char, w) => (char, bits)
          }
       
          if (bits.isEmpty) Nil
          else {     
            val pair = decodetravesal(tree, bits)
            //if (pair._1 == '') Nil else //decode failed
            pair._1 :: decode(tree, pair._2)
          }
  }
  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  //TODO:
  def decodedSecret: List[Char] = decode(frenchCode, secret)
  

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  //TODO: from text to list of bits based on Haffman tree, CodeTree here
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    //quickEncode(tree)(text)
    //@tailrec
    def encodetravesal(c: Char, tree: CodeTree): List[Bit] = tree match {
      case Fork(left, right, cs, w) =>
        if (chars(left).contains(c)) 0 :: encodetravesal(c, left)
        else 1 :: encodetravesal(c, right)
      case Leaf(char, w) => Nil
    }
    
    text.flatMap(c => encodetravesal(c, tree))
  }
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  //TODO:
  def codeBits(table: CodeTable)(char: Char): List[Bit] = 
    table.find(ctelem => ctelem._1 == char) match {
      case Some(code) => code._2
      case None => Nil
    }
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  //TODO:
  def convert(tree: CodeTree): CodeTable = tree match {
      case Fork(left, right, c, w) =>
        convert(left).map(e => (e._1, 0::e._2)):::convert(right).map(e => (e._1, 1::e._2))
      case Leaf(c, w) => List((c, Nil))
  }
  
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = 
      a:::b
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = 
      text.flatMap(c => codeBits(convert(tree))(c))
      
    //println(decodedSecret)
}

class QuickCheckCodeTree extends Properties("CodeTree") {
  
  def genStr: Gen[List[Char]] = Gen.oneOf(const(List()), genChars) // convert Gen[Char] to Char
  
  def char: Gen[Char]  = Gen.oneOf("abcdefghijklmnopqrstuvwxyz".toList)
                                  
  //there is a stackoverflow error - infinite recursion if genChars rather than genStr is used
  def genChars: Gen[List[Char]] = for {
    c <- char
    s <- genStr
  } yield c :: s
  
  def genCodeTree: Gen[Huffman.CodeTree] = Gen.oneOf(genLeaf, genFork)
  
  def genLeaf: Gen[Huffman.Leaf] = for {
    c <- arbitrary[Char] //char - a-z
    w <- arbitrary[Int]
    if (w >= 0)
  } yield new Huffman.Leaf(c, w)
  
  def genFork: Gen[Huffman.Fork] = for {
    c <- genChars //arbitrary[String]
    w <- arbitrary[Int]
    l <-genCodeTree
    r <- genCodeTree
    if (w >= 0)
  } yield new Huffman.Fork(l, r, c.toList, w)
  
  implicit lazy val arbCodeTree: Arbitrary[Huffman.CodeTree] = Arbitrary(genCodeTree)
  implicit lazy val argChars: Arbitrary[List[Char]] = Arbitrary(genChars)
  
  def stdCodeTree = //Huffman.frenchCode
    Huffman.englishCode

  def isHuffmanTree(tree: Huffman.CodeTree): Boolean = {
    def convertLeavesToList(tree: Huffman.CodeTree, depth: Int): List[(Int, Int)] = tree match {
      //convert tree to list of (frequency, depth) for all leaves
      case Huffman.Fork(l, r, cs, w) => convertLeavesToList(l, depth+1) ::: convertLeavesToList(r, depth+1)
      case Huffman.Leaf(c, w) => List((w, depth))
    }
  
    // sort list on frequency in descending and prove depth is in ascending
    //val sortedList = convertLeavesToList(tree, 0).sortWith(_._1 >= _._1).map(_._2)
    //if (sortedList.isEmpty) true
    //  !(sortedList.zip(sortedList.tail :+ Int.MaxValue).exists(e => e._1 > e._2)) //check whether depth is sorted
    //above checking would fail when there are more than two elements having same frequency
    //instead sorting on depth in ascending, then compare frequency in descending
    val sortedList = convertLeavesToList(tree, 0).sortWith(_._2 <= _._2).map(_._1)
    sortedList.isEmpty || !(sortedList.zip(sortedList.tail :+ Int.MinValue).exists(e => e._1 < e._2))
  }
  
  /*
  property("check functions on CodeTree - find invariant contract") = forAll {
  	(chars1: List[Char], chars2: List[Char]) => {
       val codeTree = Huffman.createCodeTree(chars2)
       if (chars1.foldLeft(true)((b, c) => b && (chars2.indexOf(c) != -1)))
       //makeCodeTree(createCodeTree(chars1), createCodeTree(chars2)) === createCodeTree(chars1 ++ chars2)
  	}
  }
  * 
  */

  //check all leafs' depth and their frequency (weight)
  property("check Huffman property on the generated CodeTree - char with highest frequency has lowest depth") = forAll {
    //for any CodeTree, including frenchCode and englishCode
    //(tree: Huffman.CodeTree) => //isHuffmanTree(tree) //not true, any randomly generated tree is not.
    (chars: List[Char]) => 
      isHuffmanTree(Huffman.englishCode) &&
      isHuffmanTree(Huffman.frenchCode) &&
      (chars.size == 0 || isHuffmanTree(Huffman.createCodeTree(chars))) &&
      true
  }
  
  property("decoding an encoded string should be the original string") = forAll {
    (chars: List[Char], codedChars: List[Char]) => // chars here are all a-z strings based on above definitions with implicit
       val codeTree = Huffman.createCodeTree(codedChars)
       chars.toString.equals(Huffman.decode(stdCodeTree, Huffman.encode(stdCodeTree)(chars)).toString) &&
       //replace encode with quickEncode should work too
       chars.toString.equals(Huffman.decode(stdCodeTree, Huffman.quickEncode(stdCodeTree)(chars)).toString) &&
       //(!chars.foldLeft(true)((b, c) => b && (codedChars.indexOf(c) != -1)) ||
       //chars.toString.equals(Huffman.decode(codeTree, Huffman.quickEncode(codeTree)(chars)).toString)) &&
       true
  }
}