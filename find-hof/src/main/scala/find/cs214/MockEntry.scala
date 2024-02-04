package find.cs214

sealed trait MockEntry(pth: String, next: Option[MockEntry]) extends Entry:
  def path(): String = pth
  def name(): String = pth.split("/").last
  def hasNextSibling(): Boolean = this.next.nonEmpty
  def nextSibling(): MockEntry = this.next.getOrElse(throw new NoNextSiblingException())

case class MockFile(pth: String, next: Option[MockEntry], bytes: Int) extends MockEntry(pth, next):
  def firstChild(): Entry = throw new NotADirectoryException()
  def hasChildren(): Boolean = false
  def isDirectory(): Boolean = false
  def size(): Long = bytes

case class MockDirectory(pth: String, next: Option[MockEntry], first: Option[MockEntry]) extends MockEntry(pth, next):
  def firstChild(): Entry = this.first.getOrElse(throw new NoChildrenException)
  def hasChildren(): Boolean = this.first.nonEmpty
  def isDirectory(): Boolean = true
  def size(): Long = throw new NotAFileException()
