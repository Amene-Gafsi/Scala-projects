package find

import cs214.PathMacro

import scala.meta.*

class FindHOFTests extends munit.FunSuite:
  val srcRoot = os.Path(PathMacro.sourcePath) / os.up / os.up / os.up / os.up
  val findFile = srcRoot / "main" / "scala" / "find" / "find.scala"
  val findContent = os.read(findFile)
  given Dialect = dialects.Scala33.withAllowToplevelTerms(true)
  val findAst = findContent.parse[Source].get

  def getDefs(src: Source, name: String) =
    src.collect { case d: Defn.Def if d.name.value == name => d }

  def getCallsTo(src: Defn.Def, name: String) =
    src.collect { case t: Term.Apply if t.fun.toString == name => t }

  def assertCalls(caller: String, callee: String, min: Int = 1) =
    val callerDefs = getDefs(findAst, caller)
    assert(callerDefs.nonEmpty)
    val calleeCalls = getCallsTo(callerDefs.head, callee)
    assert(calleeCalls.size >= min)

  test("a function named `findAndPrint` in `find.scala` exists (1pt)"):
    assert(getDefs(findAst, "findAndPrint").nonEmpty)

  test("`findAllAndPrint` calls `findAndPrint` (1pt)"):
    assertCalls("findAllAndPrint", "findAndPrint")

  test("`findByNameAndPrint` calls `findAndPrint` (1pt)"):
    assertCalls("findByNameAndPrint", "findAndPrint")

  test("`findBySizeEqAndPrint` calls `findAndPrint` (1pt)"):
    assertCalls("findBySizeEqAndPrint", "findAndPrint")

  test("`findBySizeGeAndPrint` calls `findAndPrint` (1pt)"):
    assertCalls("findBySizeGeAndPrint", "findAndPrint")

  test("`findEmptyAndPrint` calls `findAndPrint` (1pt)"):
    assertCalls("findEmptyAndPrint", "findAndPrint")

  test("After completing the lab, please report how long you spent on it (1pt)"):
    assert(howManyHoursISpentOnThisLab() > 0.0)
