package anagrams

import Anagrams.*
import cli.loadWordlist

class AnagramsSuite extends munit.FunSuite:
  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds

  test("computeOccurrenceList: abcd (3pts)"):
    assertEquals(computeOccurrenceList("abcd"), List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))

  test("computeOccurrenceList: Robert (3pts)"):
    assertEquals(computeOccurrenceList("Robert"), List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))

  test("computeOccurrenceList: live (3pts)"):
    assertEquals(computeOccurrenceList("live"), List(('e', 1), ('i', 1), ('l', 1), ('v', 1)))

  test("computeOccurrenceList: lovely (3pts)"):
    assertEquals(computeOccurrenceList("lovely"), List(('e', 1), ('l', 2), ('o', 1), ('v', 1), ('y', 1)))

  test("computeOccurrenceList: Léon (3pts)"):
    assertEquals(computeOccurrenceList("Léon"), List(('e', 1), ('l', 1), ('n', 1), ('o', 1)))

  test("computeOccurrenceList: Heather (3pts)"):
    assertEquals(computeOccurrenceList("Heather"), List(('a', 1), ('e', 2), ('h', 2), ('r', 1), ('t', 1)))

  test("sentenceOccurrences: abcd e (5pts)"):
    assertEquals(sentenceOccurrences(List("abcd", "e")), List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))

  test("sentenceOccurrences: Nil (5pts)"):
    assertEquals(sentenceOccurrences(List()), List())

  test("sentenceOccurrences: Roberto Carlos (5pts)"):
    assertEquals(
      sentenceOccurrences(List("Roberto", "Carlos")),
      List(('a', 1), ('b', 1), ('c', 1), ('e', 1), ('l', 1), ('o', 3), ('r', 3), ('s', 1), ('t', 1))
    )

  test("createDictionary: empty (3pts)"):
    assertEquals(createDictionary(List()), Map())

  test("createDictionary: simple (12pts)"):
    assertEquals(
      createDictionary(List("eat", "tea", "wasp", "swap"))
        .view.mapValues(_.toSet).toMap,
      Map(
        List(('a', 1), ('e', 1), ('t', 1)) ->
          Set("eat", "tea"),
        List(('a', 1), ('p', 1), ('s', 1), ('w', 1)) ->
          Set("wasp", "swap")
      )
    )

  test("createDictionary: normalization (5pts)"):
    assertEquals(
      createDictionary(List("à", "noël", "Léon", "a"))
        .view.mapValues(_.toSet).toMap,
      Map(
        List(('a', 1)) ->
          Set("à", "a"),
        List(('e', 1), ('l', 1), ('n', 1), ('o', 1)) ->
          Set("Léon", "noël")
      )
    )

  lazy val able = createDictionary(loadWordlist("en-debian"))

  test("createDictionary(...).get: eat (5pts)"):
    assertEquals(
      able.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet),
      Some(Set("ate", "eat", "tea"))
    )

  test("createDictionary(...).get: grab (10pts)"):
    assertEquals(
      able.get(List(('a', 1), ('b', 1), ('g', 1), ('r', 1))).map(_.toSet),
      Some(Set("grab", "garb", "brag"))
    )

  test("wordAnagrams player (2pts)"):
    assertEquals(wordAnagrams(able, "player").toSet, Set("parley", "pearly", "player", "replay"))

  test("word anagrams: reactive (4pts)"):
    assertEquals(wordAnagrams(able, "reactive").toSet, Set("creative", "reactive"))
  test("word anagrams: silent (4pts)"):
    assertEquals(wordAnagrams(able, "silent").toSet, Set("enlist", "inlets", "listen", "silent"))

  test("word anagrams: Easter (4pts)"):
    assertEquals(wordAnagrams(able, "Easter").toSet, Set("easter", "eaters", "Teresa"))

  test("word anagrams: Elvis (4pts)"):
    assertEquals(wordAnagrams(able, "Elvis").toSet, Set("lives", "veils", "evils", "Elvis", "Levis"))

  test("word anagrams: leaf (4pts)"):
    assertEquals(wordAnagrams(able, "leaf").toSet, Set("flea", "leaf"))

  test("subtract: lard - r (10pts)"):
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assertEquals(subtract(lard, r), lad)

  test("subtract: jimmy - my (10pts)"):
    val jimmy = List(('i', 1), ('j', 1), ('m', 2), ('y', 1))
    val my = List(('m', 1), ('y', 1))
    val jim = List(('i', 1), ('j', 1), ('m', 1))
    assertEquals(subtract(jimmy, my), jim)

  test("subtract: ok - ok (10pts)"):
    val ok = List(('k', 1), ('o', 1))
    val empty = Nil
    assertEquals(subtract(ok, ok), empty)

  test("subtract: abba - abba (10pts)"):
    val abba = List(('a', 2), ('b', 2))
    val empty = Nil
    assertEquals(subtract(abba, abba), empty)

  test("subtract: assessment - assess (10pts)"):
    val assessment = List(('a', 1), ('e', 2), ('m', 1), ('n', 1), ('s', 4), ('t', 1))
    val assess = List(('a', 1), ('e', 1), ('s', 4))
    val ment = List(('e', 1), ('m', 1), ('n', 1), ('t', 1))
    assertEquals(subtract(assessment, assess), ment)

  test("subsets: [] (8pts)"):
    assertEquals(subsets(Nil), List(Nil))

  test("subsets: abba (8pts)"):
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assertEquals(subsets(abba).toSet, abbacomb.toSet)

  test("subsets: ok (8pts)"):
    val ok = List(('k', 1), ('o', 1))
    val okcomb = List(
      List(),
      List(('o', 1)),
      List(('k', 1), ('o', 1)),
      List(('k', 1))
    )
    assertEquals(subsets(ok).toSet, okcomb.toSet)

  test("subsets: eye (8pts)"):
    val eye = List(('e', 2), ('y', 1))
    val eyecomb = List(
      List(),
      List(('e', 1)),
      List(('e', 1), ('y', 1)),
      List(('e', 2)),
      List(('e', 2), ('y', 1)),
      List(('y', 1))
    )
    assertEquals(subsets(eye).toSet, eyecomb.toSet)

  test("subsets: kokok (8pts)"):
    val kokok = List(('k', 3), ('o', 2))
    val kokokcomb = List(
      List(),
      List(('o', 1)),
      List(('o', 2)),
      List(('k', 1), ('o', 1)),
      List(('k', 2), ('o', 1)),
      List(('k', 3), ('o', 1)),
      List(('k', 1), ('o', 2)),
      List(('k', 2), ('o', 2)),
      List(('k', 3), ('o', 2)),
      List(('k', 1)),
      List(('k', 2)),
      List(('k', 3))
    )
    assertEquals(subsets(kokok).toSet, kokokcomb.toSet)

  test("sentence anagrams: [] (10pts)"):
    val sentence = List()
    assertEquals(sentenceAnagrams(able, sentence), List(Nil))

  test("sentence anagrams: Ecublens VD (10pts)"):
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assertEquals(sentenceAnagrams(able, sentence).toSet, anas.toSet)

  test("sentence anagrams: I love you (10pts)"):
    val sentence = List("I", "love", "you")

    val anas = List(
      List("Io", "you", "Lev"),
      List("love", "I", "you"),
      List("Io", "Lev", "you"),
      List("you", "Io", "Lev"),
      List("I", "you", "love"),
      List("I", "love", "you"),
      List("olive", "you"),
      List("you", "I", "love"),
      List("Lev", "you", "Io"),
      List("you", "olive"),
      List("you", "love", "I"),
      List("love", "you", "I"),
      List("you", "Lev", "Io"),
      List("Lev", "Io", "you")
    )

    assertEquals(sentenceAnagrams(able, sentence).toSet, anas.toSet)

  test("sentence anagrams: Lukas Rytz (10pts)"):
    val sentence = List("Lukas", "Rytz")
    val anas = List(
      List("Ku", "try", "Salz"),
      List("Ku", "Salz", "try"),
      List("try", "Ku", "Salz"),
      List("try", "Salz", "Ku"),
      List("Salz", "Ku", "try"),
      List("Salz", "try", "Ku"),
      List("Katz", "surly"),
      List("surly", "Katz")
    )
    assertEquals(sentenceAnagrams(able, sentence).toSet, anas.toSet)

  test("sentence anagrams: Yell Xerxes (10pts)"):
    val sentence = List("Yell", "Xerxes")
    val anas = List(
      List("Xerxes", "yell"),
      List("yell", "Xerxes"),
      List("Lyle", "Rex", "sex"),
      List("Rex", "yell", "sex"),
      List("Rex", "Lyle", "sex"),
      List("yell", "Rex", "sex"),
      List("sex", "Lyle", "Rex"),
      List("sex", "Rex", "yell"),
      List("Rex", "sex", "Lyle"),
      List("sex", "Rex", "Lyle"),
      List("sex", "yell", "Rex"),
      List("Lyle", "sex", "Rex"),
      List("yell", "sex", "Rex"),
      List("Rex", "sex", "yell"),
      List("Lyle", "Xerxes"),
      List("Xerxes", "Lyle")
    )
    assertEquals(sentenceAnagrams(able, sentence).toSet, anas.toSet)

  test("sentence anagrams: Heather (10pts)"):
    val sentence = List("Heather")

    val anas = List(
      List("a", "her", "the"),
      List("a", "the", "her"),
      List("ah", "re", "the"),
      List("ah", "her", "et"),
      List("ah", "et", "her"),
      List("ah", "the", "re"),
      List("ah", "ether"),
      List("ah", "there"),
      List("ah", "three"),
      List("ha", "re", "the"),
      List("ha", "her", "et"),
      List("ha", "et", "her"),
      List("ha", "the", "re"),
      List("ha", "ether"),
      List("ha", "there"),
      List("ha", "three"),
      List("he", "he", "art"),
      List("he", "he", "rat"),
      List("he", "he", "tar"),
      List("he", "re", "hat"),
      List("he", "her", "at"),
      List("he", "at", "her"),
      List("he", "hat", "re"),
      List("he", "art", "he"),
      List("he", "rat", "he"),
      List("he", "tar", "he"),
      List("he", "earth"),
      List("he", "hater"),
      List("he", "heart"),
      List("re", "ah", "the"),
      List("re", "ha", "the"),
      List("re", "he", "hat"),
      List("re", "hat", "he"),
      List("re", "the", "ah"),
      List("re", "the", "ha"),
      List("re", "heath"),
      List("her", "a", "the"),
      List("her", "ah", "et"),
      List("her", "ha", "et"),
      List("her", "he", "at"),
      List("her", "at", "he"),
      List("her", "et", "ah"),
      List("her", "et", "ha"),
      List("her", "the", "a"),
      List("her", "hate"),
      List("her", "heat"),
      List("her", "Thea"),
      List("hare", "the"),
      List("hear", "the"),
      List("Hera", "the"),
      List("Rhea", "the"),
      List("here", "hat"),
      List("at", "he", "her"),
      List("at", "her", "he"),
      List("et", "ah", "her"),
      List("et", "ha", "her"),
      List("et", "her", "ah"),
      List("et", "her", "ha"),
      List("hat", "he", "re"),
      List("hat", "re", "he"),
      List("hat", "here"),
      List("the", "a", "her"),
      List("the", "ah", "re"),
      List("the", "ha", "re"),
      List("the", "re", "ah"),
      List("the", "re", "ha"),
      List("the", "her", "a"),
      List("the", "hare"),
      List("the", "hear"),
      List("the", "Hera"),
      List("the", "Rhea"),
      List("hate", "her"),
      List("heat", "her"),
      List("Thea", "her"),
      List("heath", "re"),
      List("art", "he", "he"),
      List("rat", "he", "he"),
      List("tar", "he", "he"),
      List("earth", "he"),
      List("hater", "he"),
      List("heart", "he"),
      List("ether", "ah"),
      List("ether", "ha"),
      List("there", "ah"),
      List("there", "ha"),
      List("three", "ah"),
      List("three", "ha"),
      List("heather")
    )

    assertEquals(sentenceAnagrams(able, sentence).toSet, anas.toSet)

class HoursTest extends munit.FunSuite:
  // This final test checks that you filled in the `time spent`
  // question at the end of the lab.
  test("After completing the lab, please report how long you spent on it (1pt)"):
    assert(howManyHoursISpentOnThisLab() > 0.0)
