package recursion

class StringOpsTest extends munit.FunSuite:
  test("stringLength: empty string"):
    assertEquals(stringLength(""), 0)

  test("stringLength: non-empty string"):
    assertEquals(stringLength("hello"), 5)

  test("capitalizeString: empty string"):
    assertEquals(capitalizeString(""), "")

  test("capitalizeString: lower-case string"):
    assertEquals(capitalizeString("hello world"), "HELLO WORLD")

  test("capitalizeString: mixed case string"):
    assertEquals(capitalizeString("HeLLo wOrLd"), "HELLO WORLD")

  test("wordCount: empty string"):
    assertEquals(wordCount(""), 0)

  test("wordCount: 1 word with one space before"):
    assertEquals(wordCount(" hello"), 1)

  test("wordCount: 1 word with two spaces before"):
    assertEquals(wordCount("  hello"), 1)

  test("wordCount: 1 word with one space after"):
    assertEquals(wordCount("hello "), 1)

  test("wordCount: 1 word with two spaces after"):
    assertEquals(wordCount("hello  "), 1)

  test("wordCount: 1 word with one space before and after"):
    assertEquals(wordCount(" hello "), 1)

  test("wordCount: 2 words with one space between"):
    assertEquals(wordCount("hello world"), 2)

  test("wordCount: 2 words with two spaces between"):
    assertEquals(wordCount("hello  world"), 2)

  test("wordCount: 2 words with one space before and after"):
    assertEquals(wordCount(" hello world "), 2)

  test("wordCount: 3 words with one space between"):
    assertEquals(wordCount("hello world again"), 3)

  test("isBlank: empty string"):
    assertEquals(isBlank(""), true)

  test("isBlank: spaces only"):
    assertEquals(isBlank("    "), true)

  test("isBlank: non-blank string"):
    assertEquals(isBlank(" hello "), false)

  test("caesarCipher: with shift 1"):
    assertEquals(caesarCipher("abc", 1), "bcd")

  test("caesarCipher: with shift 3"):
    assertEquals(caesarCipher("xyz", 3), "abc")

  test("reverseString: empty string"):
    assertEquals(reverseString(""), "")

  test("reverseString: single word"):
    assertEquals(reverseString("hello"), "olleh")

  test("reverseString: multiple words"):
    assertEquals(reverseString("hello world"), "dlrow olleh")
