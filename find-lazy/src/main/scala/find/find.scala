package find

import find.cs214.Entry
import os.list

def entryToList(entry: cs214.Entry): LazyList[Entry] = 
  lazy val firstChild =
    if (entry.isDirectory() && entry.hasChildren()) entryToList(entry.firstChild())
    else LazyList.empty

  lazy val nextSibling: LazyList[Entry] =
    if (entry.hasNextSibling()) entryToList(entry.nextSibling())
    else LazyList.empty

  lazy val list: LazyList[Entry] = entry #:: firstChild #::: nextSibling
  list

def findLazy(entry: cs214.Entry, predicate: Entry => Boolean): LazyList[Entry] =
  lazy val listt = entryToList(entry).filter(predicate)
  listt
  //lazy val lol =
  //  if (predicate(entry)) entry #:: recurseFirstChild #::: recurseNextSibling
  // else recurseFirstChild #::: recurseNextSibling
  
def findAndPrint(entry: cs214.Entry, predicate: cs214.Entry => Boolean): Boolean =
  lazy val resultList = findLazy(entry, predicate)
  resultList.foreach(entry => println(entry.path()))
  resultList.nonEmpty

def findAllAndPrint(entry: cs214.Entry): Boolean =
  findAndPrint(entry, _ => true)

def findByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  findAndPrint(entry, _.name() == name)

def findBySizeEqAndPrint(entry: cs214.Entry, size: Long): Boolean =
  findAndPrint(entry, e => !e.isDirectory() && e.size() == size)

def findBySizeGeAndPrint(entry: cs214.Entry, minSize: Long): Boolean =
  findAndPrint(entry, e => !e.isDirectory() && e.size() >= minSize)

def findEmptyAndPrint(entry: cs214.Entry): Boolean =
  def isEmpty(e: cs214.Entry): Boolean =
    if e.isDirectory() then
      !e.hasChildren()
    else
      e.size() == 0

  findAndPrint(entry, isEmpty)

def howManyHoursISpentOnThisLab(): Double =
  2 // in hours, so put 3.5 here if you spent 3 hours and a half on the lab

def findFirstByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  lazy val result = findLazy(entry, _.name() == name)
  result.foreach(entry => println(entry.path()))
  result.nonEmpty
