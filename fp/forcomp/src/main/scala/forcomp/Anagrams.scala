package forcomp

import scala.annotation.tailrec

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.const
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

object Anagrams {
  def main(args: Array[String]) {
    val sentence = List("Scala", "is", "cool")
    println(sentenceAnagrams(sentence))
  }

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercases.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  //TODO:
  //it's important to have Occurrences sorted, since two lists are different, though they might have same elements in different order.
  // w might be empty
  def wordOccurrences(w: Word): Occurrences = 
    (w.toLowerCase().toList.groupBy(identity).mapValues(l => l.size)).toList.sorted

  /** Converts a sentence into its character occurrence list. */
  //TODO:
  def sentenceOccurrences(s: Sentence): Occurrences = 
    wordOccurrences(s.mkString(""))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  //TODO:
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = 
    dictionary.groupBy(w => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  //TODO:
  def wordAnagrams(word: Word): List[Word] = 
    dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  //TODO:
  //@tailrec
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    
    if (occurrences.isEmpty) List(List())
    else {
      val o = occurrences.head
      val hl = for (i <- 1 to o._2) yield (o._1, i)      
      val tl = combinations(occurrences.tail)
      hl.toList.flatMap(elem1 => tl.flatMap(elem2 => List(elem1::elem2))) ::: tl 
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  //TODO:
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    /*
    if (y.isEmpty) x
    else {
      val yhead = y.head
      val idx = x.indexWhere(oxi => oxi._1 == yhead._1, 0)
      val oldx = x.apply(idx)
      val newx = if (oldx._2 == yhead._2) x.dropRight(x.length - idx) ::: x.drop(idx + 1)
               else x.updated(idx, (oldx._1, oldx._2 - yhead._2))
      subtract(newx, y.tail)
    }
    * 
    */

    val sub = for {
      xelem <- x
      yelem <- y
      if (yelem._1 == xelem._1 && xelem._2 > yelem._2)
    } yield (xelem._1, xelem._2 - yelem._2)
    
    // filter common elements out
    // sort the result on the first part char?
    (sub ++ x.filter(xelem => y.indexWhere(yelem => yelem._1 == xelem._1, 0) == -1)).sortWith(_._1 < _._1)//or _2
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  //TODO:
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    //@tailrec
    def occurrencesAnagrams(os: Occurrences): List[Sentence] = {
      if (os.isEmpty) List(Nil)
      else {
        for {
             o <- combinations(os)
             w <- dictionaryByOccurrences.getOrElse(o, Nil)
             ls <- occurrencesAnagrams(subtract(os, wordOccurrences(w)))//do not use o directly, since w might be Nil
        } yield w::ls
      }
    }
    
    occurrencesAnagrams(sentenceOccurrences(sentence))
  }
}

class QuickCheckAnagrams extends Properties("FunSets.Set") {
import Anagrams._

  def genStr: Gen[List[Char]] = Gen.oneOf(const(List()), genChars) // convert Gen[Char] to Char
  
  def char: Gen[Char]  = Gen.oneOf("abcdefghijklmnopqrstuvwxyz".toList)
                                  
  //there is a stackoverflow error - infinite recursion if genChars rather than genStr is used
  def genChars: Gen[List[Char]] = for {
    c <- char
    s <- genStr
  } yield c :: s
  
  def genSentence: Gen[List[Word]] = Gen.oneOf(const(List()), genWords)
    
  def genWords: Gen[List[Word]] = for {
    w <- genStr
    s <- genSentence
  } yield w.toString :: s
  
  implicit lazy val argChars: Arbitrary[List[Char]] = Arbitrary(genChars)
  implicit lazy val argSentence: Arbitrary[List[Word]] = Arbitrary(genWords)
  
  property("check all generated sentences are legal and have the same occurrences") = forAll {
    (sentence: List[Word]) => {
      val o = sentenceOccurrences(sentence)
      /*
      sentenceAnagrams(sentence).foldLeft(true)((b, s) => b && 
              s.foldLeft(true)((b, w) => b && dictionary.indexOf(w) != -1) && 
              //sentenceOccurrences(s).toSet().diff(o.toSet).isEmpty &&
              //o.toSet.diff(sentenceOccurrences(s).toSet()).isEmpty) &&
              true)     
      *    
      */
      !sentenceAnagrams(sentence).exists { s =>
        s.exists { w => (dictionary.indexOf(w) == -1) } || 
        !o.toSet.diff(sentenceOccurrences(s).toSet).isEmpty ||
        !sentenceOccurrences(s).toSet.diff(o.toSet).isEmpty
      }
    }
  }
}
