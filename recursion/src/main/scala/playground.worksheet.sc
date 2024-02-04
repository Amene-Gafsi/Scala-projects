import recursion.*

2 + 2

val oneTwoThree = IntCons(1, IntCons(2, IntCons(3, IntNil())))

oneTwoThree.isEmpty

oneTwoThree.head

oneTwoThree.tail

oneTwoThree.tail.head

// The call below currently fails with a `NotImplementedError`. Go to
// `src/main/scala/recursion/listOps.scala`, implement the `length` function,
// and uncomment the call below to test your implementation.

// length(oneTwoThree)
