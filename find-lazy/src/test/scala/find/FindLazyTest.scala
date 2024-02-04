package find

import cs214.PathMacro

import scala.meta.*
import find.cs214.MockDirectory
import find.cs214.MockFile
import find.cs214.Entry
import find.cs214.NoNextSiblingException
import find.cs214.NotADirectoryException
import find.cs214.NotAFileException
import find.cs214.NoChildrenException
import find.cs214.CyclicDirectory

class FindLazyTest extends munit.FunSuite:
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

  val mockEntry = MockDirectory(
    "/foo",
    next = None,
    first = Some(
      MockDirectory(
        "/foo/bar",
        first =
          Some(MockFile("foo/bar/baz", next = Some(MockFile("foo/bar/buzz", None, 28)), 20)),
        next = Some(MockFile("/foo/baz", None, 10))
      )
    )
  )

  test("findLazy evaluates the predicate lazily (10pts)"):
    var count = 0
    val entries = findLazy(
      mockEntry,
      e =>
        count += 1; true
    )
    assertEquals(count, 0)

    val first = entries(0)
    assertEquals(first, mockEntry)
    assertEquals(count, 1)

    val second = entries(1)
    assertEquals(second, mockEntry.first.get)
    assertEquals(count, 2)

  test("findLazy traverse the tree lazily (5pts)"):
    val mockEntry = CountedDirectory("foo", first = Some(CountedFile("foo/bar", next = Some(CountedFile("foo/baz")))))
    val entries = findLazy(mockEntry, e => true)
    assertEquals(mockEntry.totalCount(), 0)
    val _ = entries.force
    assertEquals(mockEntry.totalCount(), 2)

  test("findLazy traverse cyclic tree lazily (10pts)"):
    val entries = findLazy(CyclicDirectory, e => true)
    val first4 = entries.take(4)

    assertEquals(first4.length, 4)
    first4.foreach(assertEquals(_, CyclicDirectory))

  test("`findFirstByNameAndPrint` calls `findLazy` (2pts)"):
    assertCalls("findFirstByNameAndPrint", "findLazy")

  test("`findAndPrint` calls `findLazy` (2pts)"):
    assertCalls("findAndPrint", "findLazy")

/** A mock Entry, which counts how many times it's visited.
  *
  * @param count
  *   How many times this node was visited (using firstChild or nextSibling)
  */
sealed trait CountedEntry(pth: String, next: Option[CountedEntry], var count: Int = 0) extends Entry:
  def path(): String = pth
  def name(): String = pth.split("/").last
  def hasNextSibling(): Boolean = this.next.nonEmpty
  def nextSibling(): CountedEntry = this.next.getOrElse(throw new NoNextSiblingException()).incremented()
  def incremented(): CountedEntry =
    this.count += 1; this
  def totalCount(): Int

case class CountedFile(pth: String, next: Option[CountedEntry] = None) extends CountedEntry(pth, next):
  def firstChild(): Entry = throw new NotADirectoryException()
  def hasChildren(): Boolean = throw new NotADirectoryException()
  def isDirectory(): Boolean = false
  def size(): Long = 42
  def totalCount(): Int = this.count + this.next.map(_.totalCount()).getOrElse(0)

case class CountedDirectory(pth: String, next: Option[CountedEntry] = None, first: Option[CountedEntry] = None)
    extends CountedEntry(pth, next):
  def firstChild(): Entry = this.first.getOrElse(throw new NoChildrenException()).incremented()
  def hasChildren(): Boolean = this.first.nonEmpty
  def isDirectory(): Boolean = true
  def size(): Long = throw new NotAFileException()
  def totalCount(): Int =
    this.count + this.next.map(_.totalCount()).getOrElse(0) + first.map(_.totalCount()).getOrElse(0)
