package find

class FindTests extends munit.FunSuite:
  // To test your implementation of `find`, we have pre-recorded the output of
  // `find` on a real system, along with file and directory sizes.
  //
  // This technique is called "mocking" — we don't want to modify the file
  // system on your machine, so instead we're running your code with a virtual
  // directory tree.
  //
  // You can see the contents of that tree below.  The listing was generated
  // with `find . -printf "%y\t%p\t%s\n"`, which prints one letter (`d` or `f`)
  // to indicate whether this line is a directory or a file, then the name of
  // the entry, and finally its size (4096 for directories on GNU/Linux).
  //
  // The `cs214.FindOutputParser` function processes this listing and converts
  // it to an entry equivalent to the one that `cs214.open` would produce.
  //
  // Bonus question: Would you know how to write the code to transform this flat
  // listing into a hierarchy of nested `Entry` objects?

  val foodRoot = cs214.FindOutputParser.parse("""
    d	food	4096
    d	food/fruits	4096
    f	food/fruits/strawberry.txt	451
    f	food/fruits/tomato.txt	1984
    d	food/vegetables	4096
    f	food/vegetables/tomato.txt	42
  """)

  // Each test below explores this hierarchy.  The tests are presented as a command line transcript:
  // the first line is a call to `find`, and subsequent lines show the expected results.
  //
  // Your code passes the tests if it prints the expected output.

  test("`find food` returns all entries (5pts)"):
    testFind("""
      $ find food
      food
      food/fruits
      food/fruits/strawberry.txt
      food/fruits/tomato.txt
      food/vegetables
      food/vegetables/tomato.txt
     """)

  /*
  test("`find food -name tomato.txt -print -quit` returns the first tomato (0pt)"):
    testFind("""
      $ find food -name tomato.txt -print -quit
      food/fruits/tomato.txt
     """)
   */

  val simpleRoot = cs214.FindOutputParser.parse("""
    d	simple	4096
    f	simple/Readme.md	7346
    d	simple/target	4096
    d	simple/target/streams	4096
    f	simple/target/streams/.agignore	0
    d	simple/target/streams/compile	4096
    d	simple/target/streams/compile/externalDependencyClasspath	4096
    d	simple/target/streams/compile/externalDependencyClasspath/_global	4096
    d	simple/target/streams/compile/externalDependencyClasspath/_global/streams	4096
    f	simple/target/streams/compile/externalDependencyClasspath/_global/streams/out	0
    d	simple/target/streams/compile/managedClasspath	4096
    d	simple/target/streams/compile/scalacOptions	4096
    d	simple/target/streams/compile/unmanagedClasspath	4096
    d	simple/target/streams/test	4096
    f	simple/target/streams/test/.agignore	0
    d	simple/target/streams/runtime	4096
    d	simple/src	4096
    d	simple/src/test	4096
    d	simple/src/test/scala	4096
    d	simple/src/test/scala/find	4096
    f	simple/src/test/scala/find/FindTest.scala	4548
    d	simple/src/test/scala/find/io	4096
    f	simple/src/test/scala/find/io/OSLibEntryTest.scala	1196
    f	simple/src/test/scala/find/io/FindOutputParser.scala	1459
    f	simple/src/test/scala/find/io/TestDir.scala	618
    f	simple/build.sbt	274
    d	simple/project	4096
    f	simple/project/plugins.sbt	55
    d	simple/images	4096
    f	simple/images/files.png	30578
    f	simple/.gitignore	0
  """)

  test("`find simple -empty` returns empty directories and files (2pts)"):
    testFind("""
      $ find simple -empty
      simple/.gitignore
      simple/target/streams/.agignore
      simple/target/streams/compile/externalDependencyClasspath/_global/streams/out
      simple/target/streams/compile/managedClasspath
      simple/target/streams/compile/scalacOptions
      simple/target/streams/compile/unmanagedClasspath
      simple/target/streams/runtime
      simple/target/streams/test/.agignore
     """)

  test("`find simple -size 0c` returns empty files only (1pt)"):
    testFind("""
      $ find simple -size 0c
      simple/.gitignore
      simple/target/streams/.agignore
      simple/target/streams/compile/externalDependencyClasspath/_global/streams/out
      simple/target/streams/test/.agignore
    """)

  test("`find simple -name foo.txt` returns no results (1pt)"):
    testFind("""
      $ find simple -name foo.txt
    """)

  test("`find simple -name simple` finds the root (1pt)"):
    testFind("""
      $ find simple -name simple
      simple
    """)

  test("`find simple -name project` finds a sibling (1pt)"):
    testFind("""
      $ find simple -name project
      simple/project
    """)

  test("`find simple -name .agignore` finds all copies of .agignore (1pt)"):
    testFind("""
      $ find simple -name .agignore
      simple/target/streams/.agignore
      simple/target/streams/test/.agignore
    """)

  test("`find simple -size 1196c` returns a single result (1pt)"):
    testFind("""
      $ find simple -size 1196c
      simple/src/test/scala/find/io/OSLibEntryTest.scala
    """)

  test("`find simple -size 1195c` returns no results (1pt)"):
    testFind("""
      $ find simple -size 1195c
    """)

  test("`find simple -size +3163c` returns multiple results (1pt)"):
    testFind("""
      $ find simple -size +3163c
      simple/Readme.md
      simple/images/files.png
      simple/src/test/scala/find/FindTest.scala
    """)

  test("`find simple -size +35163c` returns no results (1pt)"):
    testFind("""
      $ find simple -size +35163c
    """)

  // The following tests exercise corner cases in which there are no results:
  val trickyRoot = cs214.FindOutputParser.parse("""
    d	tricky	4096
    f	tricky/Readme.md	7346
  """)

  test("`find tricky -empty` returns nothing (1pt)"):
    testFind("""
      $ find tricky -empty
     """)

  test("`find tricky -name notFound` returns nothing (1pt)"):
    testFind("""
      $ find tricky -name notFound
     """)

  test("`find tricky -size 363c` returns nothing (1pt)"):
    testFind("""
      $ find tricky -size 363c
     """)

  test("`find tricky -size +35363c` returns nothing (1pt)"):
    testFind("""
      $ find tricky -size +35363c
     """)

  // This final test checks that you filled in the `time spent`
  // question at the end of the lab.
  test("`After completing the lab, please report how long you spent on it (1pt)"):
    assert(howManyHoursISpentOnThisLab() > 0.0)

  def testFind(transcript: String) =
    // Parse the test transcript into a list of arguments and an expected output string.
    val (args, expectedOutput) = cs214.FindOutputParser.parseFullTranscript(transcript)

    // Run our own implementation of `find` and store the output into `stdout`
    val stdout = new java.io.ByteArrayOutputStream()
    val found = Console.withOut(stdout) {
      cli.entryPoint(args) { path =>
        path match
          case "food"   => foodRoot
          case "simple" => simpleRoot
          case "tricky" => trickyRoot
          case _        => throw new IllegalArgumentException(f"Unknown root $path")
      }
    }

    // Check the output
    assertEquals(stdout.toString().replaceAll("\r\n", "\n"), expectedOutput.replaceAll("\r\n", "\n"))

    // Check the return value
    assertEquals(found, expectedOutput.trim != "")
