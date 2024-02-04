package find

object FindTestSetting:
  // To test your implementation of `find`, we have pre-recorded the output of
  // `find` on a real system, along with file and directory sizes.
  //
  // This technique is called "mocking" — we don't want to modify the file
  // system on your machine, so instead we're running your code with a virtual
  // directory tree.
  //
  // You can see the contents of that tree below.  The listing was generated
  // with `find . -printf "%y\t%p\t%s\n"`, which prints one letter (`d` or `f`)
  // to indicate whether this line is a directory or a file, then the name of
  // the entry, and finally its size (4096 for directories on GNU/Linux).
  //
  // The `cs214.FindOutputParser` function processes this listing and converts
  // it to an entry equivalent to the one that `cs214.open` would produce.
  //

  // start food
 /* val foodRoot = cs214.FindOutputParser.parse("""
    d	food	4096
    d	food/fruits	4096
    f	food/fruits/strawberry.txt	451
    f	food/fruits/tomato.txt	1984
    d	food/vegetables	4096
    f	food/vegetables/tomato.txt	42
  """)
  // end food

  // start setting
  val queryFileName = "tomato.txt"
  val correctOutputFiles = List("food/fruits/tomato.txt", "food/vegetables/tomato.txt") */
  // end setting

  val foodRoot = cs214.FindOutputParser.parse("""
    d	food	4096
    d	food/fruits	4096
    f	food/fruits/strawberry.txt	451
    f	food/fruits/tomato.txt	1984
    d	food/vegetables	4096
    f	food/fruits/strawberry.txt	451

  """)
  // end food

  // start setting
  val queryFileName = "tomato.txt"
  val correctOutputFiles = List("food/fruits/tomato.txt" )