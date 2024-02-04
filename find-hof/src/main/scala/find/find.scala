package find

def findAndPrint(entry : cs214.Entry, f:(cs214.Entry) => Boolean): Boolean = 
  
  val thisFound = f(entry) && { println(entry.path()); true }   

  val childrenFound =
    entry.isDirectory()
      && entry.hasChildren()
      && findAndPrint(entry.firstChild(), f)

  val nextSiblingsFound =
    entry.hasNextSibling()
      && findAndPrint(entry.nextSibling(), f)

  thisFound || childrenFound || nextSiblingsFound


def findAllAndPrint(entry: cs214.Entry): Boolean =

  findAndPrint(entry, _ => true)

 // println(entry.path())
  //if entry.isDirectory() && entry.hasChildren() then
   // findAllAndPrint(entry.firstChild())
  //if entry.hasNextSibling() then
   // findAllAndPrint(entry.nextSibling())

  //true

def findByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  findAndPrint(entry, (entry) => entry.name() == name)

//  val thisFound =
 //   entry.name() == name
  //    && { println(entry.path()); true }

  //val childrenFound =
//    entry.isDirectory()
    //  && entry.hasChildren()
      //&& findByNameAndPrint(entry.firstChild(), name)

  //val nextSiblingsFound =
    //entry.hasNextSibling()
    //  && findByNameAndPrint(entry.nextSibling(), name)

  //thisFound || childrenFound || nextSiblingsFound

def findBySizeEqAndPrint(entry: cs214.Entry, size: Long): Boolean =
  findAndPrint(entry, (entry) => (!entry.isDirectory()) && (entry.size() == size))

//  val thisFound =
  //  !entry.isDirectory()
    //  && entry.size() == size
      //&& { println(entry.path()); true }

  //val childrenFound =
//    entry.isDirectory()
    //  && entry.hasChildren()
      //&& findBySizeEqAndPrint(entry.firstChild(), size)

  //val nextSiblingsFound =
    //entry.hasNextSibling()
    //  && findBySizeEqAndPrint(entry.nextSibling(), size)

  //thisFound || childrenFound || nextSiblingsFound

def findBySizeGeAndPrint(entry: cs214.Entry, minSize: Long): Boolean =
  findAndPrint(entry, (entry) => (!entry.isDirectory()) && (entry.size() >= minSize))

//  val thisFound =
 //   !entry.isDirectory()
  //    && entry.size() >= minSize
   //   && { println(entry.path()); true }

 // val childrenFound =
  //  entry.isDirectory()
   //   && entry.hasChildren()
    //  && findBySizeGeAndPrint(entry.firstChild(), minSize)

  //val nextSiblingsFound =
    //entry.hasNextSibling()
      //&& findBySizeGeAndPrint(entry.nextSibling(), minSize)

  //thisFound || childrenFound || nextSiblingsFound

def findEmptyAndPrint(entry: cs214.Entry): Boolean =
  findAndPrint(entry, (entry) => ((entry.isDirectory()) && (!entry.hasChildren())) || 
                                 ((!entry.isDirectory()) && (entry.size() == 0)))

 // val isEmpty =
  //  if entry.isDirectory() then
   //   !entry.hasChildren()
    //else
//      entry.size() == 0

  //val thisFound =
    //isEmpty && { println(entry.path()); true }

  //val childrenFound =
    //entry.isDirectory()
      //&& entry.hasChildren()
      //&& findEmptyAndPrint(entry.firstChild())

  //val nextSiblingsFound =
    //entry.hasNextSibling()
      //&& findEmptyAndPrint(entry.nextSibling())

 // thisFound || childrenFound || nextSiblingsFound

def howManyHoursISpentOnThisLab(): Double =
  5 // in hours, so put 3.5 here if you spent 3 hours and a half on the lab

def findFirstByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  val thisFound =
    entry.name() == name
      && { println(entry.path()); true }

  def childrenFound =
    entry.isDirectory()
      && entry.hasChildren()
      && findFirstByNameAndPrint(entry.firstChild(), name)

  def nextSiblingsFound =
    entry.hasNextSibling()
      && findFirstByNameAndPrint(entry.nextSibling(), name)

  thisFound || childrenFound || nextSiblingsFound
