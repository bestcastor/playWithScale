
/** A word is simply a `String`. */
type Word = String

/** A sentence is a `List` of words. */
type Sentence = List[Word]

/** `Occurrences` is a `List` of pairs of characters and positive integers saying
  *  how often the character appears.
  *  This list is sorted alphabetically w.r.t. to the character in each pair.
  *  All characters in the occurrence list are lowercase.
  *
  *  Any list of pairs of lowercase characters and their frequency which is not sorted
  *  is **not** an occurrence list.
  *
  *  Note: If the frequency of some character is zero, then that character should not be
  *  in the list.
  */
type Occurrences = List[(Char, Int)]


val word = "NannanYu"

val sentence = List("NannanYu", "love",  "Lee")

val dictionary = List("eat", "tea", "lee", "ele")


/** Converts the word into its character occurrence list.
  *
  *  Note: the uppercase and lowercase version of the character are treated as the
  *  same character, and are represented as a lowercase character in the occurrence list.
  *
  *  Note: you must use `groupBy` to implement this method!
  */
def wordOccurrences(w: Word): Occurrences = {
  ((w.toLowerCase map (c => (c, 1))).toList groupBy (_._1)
    map (key => (key._1, key._2.length))).toList sorted
}

val occurrences = List(('a', 2), ('b', 2))
val occurrences1 = List(('a', 5), ('b', 6), ('c', 1))

for (
  (c, num) <- occurrences1
) yield {
  var res = num
  occurrences.exists(b => {
    val eq = (b._1 == c)
    if (eq) res = num - b._2
    eq
  })
  (c, res)
}

wordOccurrences(word)

val abc = List('a', 'b', 'c')

//val ocs : List[Occurrences] = ???

def onlyOneOccur(list : List[(Char, Int)]): Boolean = {
  val group = list.groupBy(_._1)
  if (group.size != list.length) false
  else true
}
val ocs = occurrences.map( x => (for(i <- 1 to x._2) yield (x._1,i)).toList).flatten

ocs.toSet[(Char, Int)].subsets.map(_.toList).toList filter onlyOneOccur



/** Converts a sentence into its character occurrence list. */
def sentenceOccurrences(s: Sentence): Occurrences =
  wordOccurrences(s mkString "")

sentenceOccurrences(sentence)

