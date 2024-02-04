package recursion

class ListOpsTests extends munit.FunSuite:
  val emptyList: IntList = makeList()
  val twoElemList: IntList = makeList(1, 2)

  test("length: empty list"):
    assertEquals(length(emptyList), 0)

  test("length: list with 2 elements"):
    assertEquals(length(twoElemList), 2)

  test("sum: empty list"):
    assertEquals(sum(emptyList), 0)

  test("sum: list with 2 elements"):
    assertEquals(sum(twoElemList), 3)

  test("product: empty list"):
    assertEquals(product(emptyList), 1)

  test("product: list with 2 elements"):
    assertEquals(product(twoElemList), 2)

  test("horner: empty list"):
    assertEquals(horner(emptyList, 3), 0)

  test("horner: list with 2 elements"):
    assertEquals(horner(twoElemList, 3), 7)

  test("allEven: empty list"):
    assertEquals(allEven(emptyList), true)

  test("allEven: list with 2 elements"):
    assertEquals(allEven(twoElemList), false)

  test("allPositiveOrZero: empty list"):
    assertEquals(allPositiveOrZero(emptyList), true)

  test("allPositiveOrZero: list with 2 elements"):
    assertEquals(allPositiveOrZero(makeList(-1, 2)), false)

  test("anyOdd: empty list"):
    assertEquals(anyOdd(emptyList), false)

  test("anyOdd: list with 2 elements"):
    assertEquals(anyOdd(twoElemList), true)

  test("anyNegative: empty list"):
    assertEquals(anyNegative(emptyList), false)

  test("anyNegative: list with 2 elements"):
    assertEquals(anyNegative(makeList(-1, 2)), true)

  test("countEven: empty list"):
    assertEquals(countEven(emptyList), 0)

  test("countEven: list with 2 elements"):
    assertEquals(countEven(twoElemList), 1)

  test("countEven2: empty list"):
    assertEquals(countEven2(emptyList), 0)

  test("countEven2: list with 2 elements"):
    assertEquals(countEven2(twoElemList), 1)

  test("countPositive: empty list"):
    assertEquals(countPositive(emptyList), 0)

  test("countPositive: list with 2 elements"):
    assertEquals(countPositive(twoElemList), 2)

  test("increment: empty list"):
    assertListEquals(increment(emptyList), makeList())

  test("increment: list with 2 elements"):
    assertListEquals(increment(twoElemList), makeList(2, 3))

  test("decrement: empty list"):
    assertListEquals(decrement(emptyList), makeList())

  test("decrement: list with 2 elements"):
    assertListEquals(decrement(twoElemList), makeList(0, 1))

  test("collectEven: empty list"):
    assertListEquals(collectEven(emptyList), makeList())

  test("collectEven: list with 2 elements"):
    assertListEquals(collectEven(twoElemList), makeList(2))

  test("removeOdd: empty list"):
    assertListEquals(removeOdd(emptyList), makeList())

  test("removeOdd: list with 2 elements"):
    assertListEquals(removeOdd(twoElemList), makeList(2))

  test("polishEval: addition"):
    val xs = makeList(Add, 1, 2)
    assertEquals(polishEval(xs)._1, 3)

  test("polishEval: nested operations"):
    val xs = makeList(Add, 2, Multiply, 3, 4)
    assertEquals(polishEval(xs)._1, 14)

  test("reverseAppend: empty lists"):
    assertEquals(reverseAppend(emptyList, emptyList), emptyList)

  test("reverseAppend: non-empty list and empty list"):
    val l1 = makeList(1, 2, 3)
    assertEquals(reverseAppend(l1, emptyList), makeList(3, 2, 1))

  test("reverseAppend: two elements to an empty list"):
    assertEquals(reverseAppend(twoElemList, emptyList), makeList(2, 1))

  test("reverseAppend: empty list to two elements"):
    assertEquals(reverseAppend(emptyList, twoElemList), twoElemList)

  test("reverseAppend: two lists"):
    val l1 = makeList(1, 2, 3)
    val l2 = makeList(4, 5)
    assertEquals(reverseAppend(l1, l2), makeList(3, 2, 1, 4, 5))

  test("reverse: empty list"):
    assertEquals(reverse(emptyList), emptyList)

  test("reverse: two elements"):
    assertEquals(reverse(twoElemList), makeList(2, 1))

  test("last: empty list"):
    intercept[IllegalArgumentException]:
      last(emptyList)

  test("last: two elements"):
    assertEquals(last(twoElemList), 2)

  test("minMax: empty list"):
    intercept[IllegalArgumentException]:
      minMax(emptyList)

  test("minMax: two elements"):
    assertEquals(minMax(twoElemList), (1, 2))

  def makeList(values: Int*): IntList =
    if values.isEmpty then new IntNil()
    else new IntCons(values.head, makeList(values.tail*))

  def assertListEquals(list1: IntList, list2: IntList): Unit =
    if list1.isEmpty && list2.isEmpty then
      // Both lists are empty, so they are equal.
      return

    if list1.isEmpty || list2.isEmpty then
      // One of the lists is empty and the other is not, so they are not equal.
      fail("One of the lists is shorter than the other.")

    if list1.head != list2.head then
      // The current elements in both lists are different.
      fail(s"List elements differ. Expected: ${list1.head}, but found: ${list2.head}.")

    // Continue the comparison with the tails of both lists.
    assertListEquals(list1.tail, list2.tail)
