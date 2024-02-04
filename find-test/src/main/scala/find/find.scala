package find

def findByNameAndPrint_Good(entry: cs214.Entry, name: String): Boolean =
  val thisFound =
    entry.name() == name
      && { println(entry.path()); true }

  val childrenFound =
    entry.isDirectory()
      && entry.hasChildren()
      && findByNameAndPrint_Good(entry.firstChild(), name)

  val nextSiblingsFound =
    entry.hasNextSibling()
      && findByNameAndPrint_Good(entry.nextSibling(), name)

  thisFound || childrenFound || nextSiblingsFound

def findByNameAndPrint_Bad(entry: cs214.Entry, name: String): Boolean =

  def found(entry: cs214.Entry) = entry.name() == name && { println(entry.path()); true }

  val thisFound = found(entry)

  val childrenFound =
    entry.isDirectory()
      && entry.hasChildren()
      && findByNameAndPrint_Bad(entry.firstChild(), name)

  val nextSiblingsFound =
    if entry.isDirectory() then entry.hasNextSibling() && findByNameAndPrint_Bad(entry.nextSibling(), name)
    else entry.hasNextSibling() && found(entry.nextSibling())

  thisFound || childrenFound || nextSiblingsFound
