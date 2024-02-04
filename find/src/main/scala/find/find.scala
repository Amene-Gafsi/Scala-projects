package find

def findAllAndPrint(entry: cs214.Entry): Boolean =
  println(entry.path())
  var foundFile = false

  if entry.isDirectory() then 
    if entry.hasChildren() then foundFile = findAllAndPrint(entry.firstChild()) || foundFile
    if entry.hasNextSibling() then foundFile = findAllAndPrint(entry.nextSibling()) || foundFile
  else 
    foundFile = true
    if entry.hasNextSibling() then findAllAndPrint(entry.nextSibling())
   
  foundFile
 
def findByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  var foundName = false

  if entry.name() == name then {
    println(entry.path())
    foundName = true
    }
  if entry.isDirectory() then 
    if entry.hasChildren() then foundName = findByNameAndPrint(entry.firstChild(), name) || foundName
    if entry.hasNextSibling() then foundName = findByNameAndPrint(entry.nextSibling(), name) || foundName
  else {
    if entry.hasNextSibling() then foundName = findByNameAndPrint(entry.nextSibling(), name) || foundName
  }
  foundName

def findBySizeEqAndPrint(entry: cs214.Entry, size: Long): Boolean =
  var foundSize = false

  if entry.isDirectory() then 
    if entry.hasChildren() then foundSize = findBySizeEqAndPrint(entry.firstChild(), size) || foundSize
    if entry.hasNextSibling() then foundSize = findBySizeEqAndPrint(entry.nextSibling(), size) || foundSize
  else 
    if entry.size() == size then {
      println(entry.path())
      foundSize = true
    }
    if entry.hasNextSibling() then foundSize = findBySizeEqAndPrint(entry.nextSibling(), size) || foundSize   
  foundSize

def findBySizeGeAndPrint(entry: cs214.Entry, minSize: Long): Boolean =
  var foundMinSize = false

  if entry.isDirectory() then 
    if entry.hasChildren() then foundMinSize = findBySizeGeAndPrint(entry.firstChild(), minSize) || foundMinSize
    if entry.hasNextSibling() then foundMinSize = findBySizeGeAndPrint(entry.nextSibling(), minSize) || foundMinSize
  else 
    if entry.size() >= minSize then {
      println(entry.path())
      foundMinSize = true
    }
    if entry.hasNextSibling() then foundMinSize = findBySizeGeAndPrint(entry.nextSibling(), minSize) || foundMinSize
   
  foundMinSize

def findEmptyAndPrint(entry: cs214.Entry): Boolean =
  var foundEmpty = false

  if entry.isDirectory() then 

    if !entry.hasChildren() then {
        println(entry.path())
        foundEmpty = true
    } else foundEmpty = findEmptyAndPrint(entry.firstChild()) || foundEmpty

    if entry.hasNextSibling() then foundEmpty = findEmptyAndPrint(entry.nextSibling()) || foundEmpty
  else 
    if entry.size() == 0 then {
      println(entry.path())
      foundEmpty = true
    }
    if entry.hasNextSibling() then findEmptyAndPrint(entry.nextSibling()) || foundEmpty
   
  foundEmpty

def howManyHoursISpentOnThisLab(): Double =
  6 // in hours, so put 3.5 here if you spent 3 hours and a half on the lab

///////////////////////////////
// The following is optional //
///////////////////////////////

def findFirstByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  println("/some/path1")
  println("/some/path2")
  true
