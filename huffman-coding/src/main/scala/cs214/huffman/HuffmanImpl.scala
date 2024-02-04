package cs214.huffman

import scala.compiletime.ops.boolean
import scala.annotation.newMain

trait HuffmanImpl[T] extends HuffmanBase[T]:

  // Part 1: Basics

  // start weight
  def weight(tree: CodeTree[T]): Int = 
    tree match
      case Leaf(_, weight) => weight
      case Fork(_, _, _, weight) => weight

  // end weight

  // start symbols
  def symbols(tree: CodeTree[T]): List[T] = 
    tree match
      case Leaf(symbol, _) => List(symbol)
      case Fork(_, _, symbols, _) => symbols
  // end symbols

  // start makeCodeTree
  def makeCodeTree(left: CodeTree[T], right: CodeTree[T]): CodeTree[T] =
    Fork(left, right, symbols(left) ++ symbols(right), weight(left) + weight(right))
  // end makeCodeTree

  // Part 2: Constructing Huffman trees

  // start symbolFreqs
  def symbolFreqs(symbols: List[T]): List[(T, Int)] = 
    def symbolFreqsHelper(
      symbols: List[T],
      frequencyMap: Map[T, Int]
  ): Map[T, Int] = {
    symbols match {
      case Nil => frequencyMap
      case head :: tail =>
        val updatedFrequencyMap = frequencyMap.updatedWith(head) {
          case Some(count) => Some(count + 1)
          case None => Some(1)
        }
        symbolFreqsHelper(tail, updatedFrequencyMap)
    }
  }
    val frequencyMap = symbolFreqsHelper(symbols, Map.empty[T, Int])
    frequencyMap.toList 

  // end symbolFreqs

  def howMany(symbol : T , symbols: List[T]): Int =
    def countTailRec(symbols: List[T], count: Int): Int = 
    if (symbols.isEmpty) {
      count
    } else {
      val newCount = if (symbols.head == symbol) count + 1 else count
      countTailRec(symbols.tail, newCount)
    }
    countTailRec(symbols, 0)

  def alreadyIn[T](symbol: T, symbols: List[(T, Int)]): Boolean =
    if (symbols.isEmpty) false
    else (symbols.head._1 == symbol) || alreadyIn(symbol, symbols.tail)

  // start makeOrderedLeafList
  def makeOrderedLeafList(freqs: List[(T, Int)]): List[Leaf[T]] = 
    freqs.sortBy { case (_, weight) => weight }.map { case (symbol, weight) => Leaf(symbol, weight) }
  // end makeOrderedLeafList

  // start isSingleton
  def isSingleton(trees: List[CodeTree[T]]): Boolean = 
    trees match
      case _ :: Nil => true  
      case _ => false 
  // end isSingleton

  def insertByWeight(tree: CodeTree[T], trees: List[CodeTree[T]]): List[CodeTree[T]] = 
    val (before, after) = trees.span(t => weight(t) <= weight(tree))
    before ++ (tree :: after)
  
  // start combine
  def combine(trees: List[CodeTree[T]]): List[CodeTree[T]] = 
    if (trees.length < 2) trees 
    else 
      val sortedTrees = trees.sortBy(weight)
      val (lowestWeightTree1, lowestWeightTree2) = (sortedTrees.head, sortedTrees.tail.head)
      val remainingTrees = sortedTrees.drop(2)
      val mergedTree = makeCodeTree(lowestWeightTree1, lowestWeightTree2)
      insertByWeight(mergedTree, remainingTrees)
  
  // end combine

  // start until
  def until(isDone: List[CodeTree[T]] => Boolean, merge: List[CodeTree[T]] => List[CodeTree[T]])(trees: List[CodeTree[T]]): List[CodeTree[T]] = 
    if (isDone(trees)) trees
    else
        val mergedTrees = merge(trees) 
        until(isDone, merge)(mergedTrees)
      

  // end until

  // start createCodeTree
  def createCodeTree(symbols: List[T]): CodeTree[T] = 
    val symbolFreqsList = symbolFreqs(symbols)
    val orderedLeafList = makeOrderedLeafList(symbolFreqsList) 
    val finalTree = until(isSingleton, combine)(orderedLeafList)
    finalTree.head
    
  // end createCodeTree

  // Part 3: Decoding
  // Reminder: type Bit = Int

  // start decodeOne
  def decodeOne(tree: CodeTree[T], bits: List[Bit]): Option[(T, List[Bit])] = 
   tree match 
    case Leaf(symbol, _) =>
      Some((symbol, bits))

    case Fork(left, right, _, _) if bits.isEmpty =>
      None

    case Fork(left, right, _, _) =>
      bits.head match 
        case 0 => decodeOne(left, bits.tail)
        case 1 => decodeOne(right, bits.tail)
      
  
  // end decodeOne

  // start decode
  def decode(tree: CodeTree[T], bits: List[Bit]): List[T] = {
    def decodeBits(currentBits: List[Bit], currentTree: CodeTree[T], result: List[T]): List[T] = {
      if (currentBits.isEmpty) {
        result.reverse
      } else {
        decodeOne(currentTree, currentBits) match {
          case Some((symbol, remainingBits)) =>
            decodeBits(remainingBits, tree, symbol :: result)
          case None =>
            result.reverse
        }
      }
    }
    decodeBits(bits, tree, List())
  }
  // end decode

  // Part 4a: Encoding using Huffman tree

  // start encode
  def encode(tree: CodeTree[T])(text: List[T]): List[Bit] = 
    def helper(tree: CodeTree[T], text : List[T], encodedBits: List[Bit]): List[Bit] = 
      text match
        case head :: next => symbolEncoding(tree, head, encodedBits) ++ encode(tree)(next)
        case Nil => encodedBits

    helper(tree, text, List())
          
  def symbolEncoding(tree: CodeTree[T], symbol : T, encodedBits: List[Bit]): List[Bit] = 
    tree match
      case Leaf(_, _) => encodedBits
      case Fork(left, right, _, _) => 
        if (symbols(left).contains(symbol)) symbolEncoding(left, symbol ,encodedBits :+ 0)
        else 
          symbolEncoding(right, symbol,encodedBits :+ 1)
    
  // end encode

  // Part 4b: Encoding using code table

  // Reminder: type CodeTable = List[(T, List[Bit])]

  // start codeBits
  
  def codeBits(table: CodeTable)(symbol: T): List[Bit] = 
    
    table.collectFirst {
    case (sym, bits) if sym == symbol => bits}.getOrElse(List())
  // end codeBits

  // start convert
  def convert(tree: CodeTree[T]): CodeTable = 
    def helper(currentTree: CodeTree[T], currentBits: List[Bit]): CodeTable = 
    currentTree match 
      case Leaf(symbol, _) => List((symbol, currentBits))
      case Fork(left, right, _, _) =>
        val leftTable = helper(left, currentBits :+ 0)
        val rightTable = helper(right, currentBits :+ 1)
        mergeCodeTables(leftTable, rightTable)
    helper(tree, List())
  // end convert

  // start mergeCodeTables
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ++ b
  
  // end mergeCodeTables

  // start quickEncode
  def quickEncode(tree: CodeTree[T])(text: List[T]): List[Bit] = 
    val table = convert(tree)
    text.flatMap(codeBits(table))
