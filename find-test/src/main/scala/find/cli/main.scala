package find
package cli

/** Command-line interface for the `find` program.
  *
  * Note: this function is provided to you and you do not need to understand how
  * it works yet.
  *
  * @param args
  *   the command-line arguments.
  */
@main def cs214find(args: String*) =
  entryPoint(args)(cs214.open)

def entryPoint(args: Seq[String])(open: String => cs214.Entry): Boolean =
  if args.length < 1 then
    return fail("No path argument given.")

  val path = args.head
  val expr = args.tail

  val file =
    try
      open(path)
    catch
      case _: java.nio.file.NoSuchFileException =>
        return fail(f"Path '$path' does not exist.")
      case e: Exception =>
        return fail(f"Open raised an exception: ${e.getMessage()}.")

  val SIZE_RE = "([+]?)([0-9]+)c".r

  expr match
    case Seq("-name", name, "-good") =>
      findByNameAndPrint_Good(file, name)

    case Seq("-name", name, "-bad") =>
      findByNameAndPrint_Bad(file, name)

    case _ =>
      return fail("Incorrect usage.")

def fail(msg: String): Boolean =
  val usage = "Usage: find path [-empty|-name N|-size Xc|-size +Xc]"
  System.err.println(f"Error: $msg\n$usage")
  false
