package find
import cli.cs214find

class FindIOTest extends munit.FunSuite:
  case class TmpDir(root: os.Path):
    val child1 = root / "child1"
    val child11 = root / "child1" / "child11"
    val child12 = root / "child1" / "child12.txt"
    val child1foo = root / "child1" / "foo.txt"
    val child2 = root / "child2"
    val child21 = root / "child2" / "child21.txt"
    val child2foo = root / "child2" / "foo.txt"

    os.makeDir(child1)
    os.makeDir(child11)
    os.write(child12, "Hello")
    os.write(child1foo, "")
    os.makeDir(child2)
    os.write(child21, "")
    os.write(child2foo, "Hello world!")

  var tmp: TmpDir = null

  override def beforeAll(): Unit =
    val root = os.temp.dir(deleteOnExit = false)
    println(f"Test dir: $root")
    tmp = TmpDir(root)

  override def afterAll(): Unit =
    os.remove.all(tmp.root)

  test("find empty dirs in test-dir works"):
    testFind(List("-empty"), List(tmp.child11, tmp.child1foo, tmp.child21))

  test("find by name foo.txt in test-dir works"):
    testFind(List("-name", "foo.txt"), List(tmp.child1foo, tmp.child2foo))

  test("find by size 5c in test-dir works"):
    testFind(List("-size", "5c"), List(tmp.child12))

  test("find by size +5c in test-dir works"):
    testFind(List("-size", "+5c"), List(tmp.child12, tmp.child2foo))

  def testFind(args: List[String], expectedFiles: List[os.Path]) =
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) { cs214find((tmp.root.toString() :: args)*) }
    assertEquals(out.toString().replaceAll("\r\n", "\n"), expectedFiles.map(_.toString()).mkString("", "\n", "\n"))
