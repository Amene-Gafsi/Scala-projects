package find.cs214

sealed trait MockEntry(pth: String) extends Entry:
  def path(): String = pth
  def name(): String = pth.split("/").last

case class MockFile(pth: String, next: Option[MockEntry], bytes: Int) extends MockEntry(pth):
  def firstChild(): Entry = throw new NotADirectoryException()
  def hasChildren(): Boolean = throw new NotAFileException()
  def isDirectory(): Boolean = false
  def size(): Long = bytes
  def hasNextSibling(): Boolean = this.next.nonEmpty
  def nextSibling(): MockEntry = this.next.getOrElse(throw new NoNextSiblingException())

case class MockDirectory(pth: String, next: Option[MockEntry], first: Option[MockEntry]) extends MockEntry(pth):
  def firstChild(): Entry = this.first.getOrElse(throw new NoChildrenException())
  def hasChildren(): Boolean = this.first.nonEmpty
  def isDirectory(): Boolean = true
  def size(): Long = throw new NotAFileException()
  def hasNextSibling() = this.next.nonEmpty
  def nextSibling() = this.next.getOrElse(throw new NoNextSiblingException())

object CyclicDirectory extends MockEntry("youShallNotPass"):
  def firstChild() = this
  def hasChildren() = true
  def isDirectory() = true
  def size(): Long = throw new NotAFileException()
  def hasNextSibling() = true
  def nextSibling() = this
