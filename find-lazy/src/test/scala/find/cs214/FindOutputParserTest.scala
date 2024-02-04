package find.cs214

import FindOutputParser.*

class FindOutputParserTest extends munit.FunSuite:
  test("splitRows() handles empty strings"):
    assertEquals(splitRows("").toList, List())

  test("splitRows() throws on malformed strings"):
    intercept[InvalidLineFormatException]:
      splitRows("a")

  test("splitRows() discards trailing and empty newlines"):
    assertEquals(splitRows("d\ta\t1\n\nf\tb\t2\n").toList, List(Row(true, "a", 1), Row(false, "b", 2)))

  test("gatherEntries() handles empty lists"):
    assertEquals(gatherEntries(List(), ""), (None, List()))

  test("gatherEntries() properly sequences siblings"):
    val rows = List(
      Row(false, "a/b", 5),
      Row(false, "a/c", 4),
      Row(false, "a/d", 3)
    )
    assertEquals(
      gatherEntries(rows, "a/"),
      (Some(MockFile("a/b", Some(MockFile("a/c", Some(MockFile("a/d", None, 3)), 4)), 5)), List())
    )

  test("gatherEntries() consumes the right prefix"):
    val rows = List(
      Row(false, "a/b", 5),
      Row(true, "d", 4096)
    )
    assertEquals(gatherEntries(rows, "a/"), (Some(MockFile("a/b", None, 5)), List(Row(true, "d", 4096))))

  test("gatherEntries() correctly identifies children"):
    val rows = List(
      Row(true, "src/a", 4096),
      Row(false, "src/a/b", 4),
      Row(false, "src/a/c", 3),
      Row(false, "src/d", 2),
      Row(false, "dst", 1)
    )
    assertEquals(
      gatherEntries(rows, "src/"),
      (
        Some(MockDirectory(
          "src/a",
          Some(MockFile("src/d", None, 2)),
          Some(MockFile("src/a/b", Some(MockFile("src/a/c", None, 3)), 4))
        )),
        List(Row(false, "dst", 1))
      )
    )
