package find

import find.FindTestSetting.*

class FindTests extends munit.FunSuite:
  // start tests
  val expectedOutput_str = correctOutputFiles.mkString("\n") + "\n"

  test("The test should tell the good and the bad versions apart (100pts)"):
    assertEquals(runFindByName("-good"), expectedOutput_str) // find_Good should be correct
    assertNotEquals(runFindByName("-bad"), expectedOutput_str) // find_Bad should return the wrong output
  // end tests

  def runFindByName(flag: String) =
    val args = List("food", "-name", queryFileName, flag)

    // Run our own implementation of `find` and store the output into `stdout`
    val stdout = new java.io.ByteArrayOutputStream()
    val found = Console.withOut(stdout) {
      cli.entryPoint(args) { path =>
        path match
          case "food" => foodRoot
          case _      => throw new IllegalArgumentException(f"Unknown root $path")
      }
    }

    stdout.toString().replaceAll("\r\n", "\n")
