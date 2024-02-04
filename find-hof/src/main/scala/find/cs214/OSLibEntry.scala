package find.cs214

class OSLibEntry(private val siblings: IndexedSeq[os.Path], private val base: Option[os.Path], private val index: Int)
    extends Entry:
  private val file = siblings(index)
  private lazy val children = os.list(file)

  override def name(): String =
    file.last

  override def size(): Long =
    if isDirectory() then throw NotAFileException()
    else os.size(file)

  override def path(): String =
    base match
      case None       => file.toString()
      case Some(base) => file.relativeTo(base).toString()

  override def hasNextSibling(): Boolean =
    index < siblings.length - 1

  override def nextSibling(): Entry =
    if !hasNextSibling() then throw NoNextSiblingException()
    OSLibEntry(siblings, base, index + 1)

  override def isDirectory(): Boolean =
    os.isDir(file)

  override def hasChildren(): Boolean =
    if !isDirectory() then throw NotADirectoryException()
    children.nonEmpty

  override def firstChild(): Entry =
    if !isDirectory() then throw NotADirectoryException()
    if children.isEmpty then throw NoChildrenException()
    OSLibEntry(children, base, 0)
