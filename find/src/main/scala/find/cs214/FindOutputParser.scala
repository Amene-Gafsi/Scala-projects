package find.cs214

// Use `find . -printf "%y\t%p\t%s\n"` to generate parseable output.

object FindOutputParser:
  class InvalidLineFormatException(line: String)
      extends Exception(s"Incorrectly formatted line ${line}.")
  class InvalidTranscriptFormatException()
      extends Exception(s"Incorrectly formatted transcript.")
  class EmptyInputException()
      extends Exception(s"No inputs found.")

  case class Row(isDir: Boolean, path: String, bytes: Int)

  def splitRows(text: String): Array[Row] =
    for line <- text.split("[\r\n]+") if !(line == null || line.trim().isEmpty())
    yield line.trim.split("\t") match
      case Array(kind, path, bytes) => Row(kind == "d", path, bytes.toInt)
      case _                        => throw new InvalidLineFormatException(line)

  def gatherEntries(rows: List[Row], parent: String = ""): (Option[MockEntry], List[Row]) =
    rows match
      case Row(isDir, path, bytes) :: rs if path.startsWith(parent) =>
        val (firstChild, rs0) = gatherEntries(rs, path + "/")
        val (firstSibling, rs1) = gatherEntries(rs0, parent)
        val entry = isDir match
          case true  => MockDirectory(path, firstSibling, firstChild)
          case false => assert(firstChild.isEmpty); MockFile(path, firstSibling, bytes)
        (Some(entry), rs1)
      case _ => (None, rows)

  def parse(output: String): Entry =
    val rows = splitRows(output.replace("(?m)^ +", "")).toList
    val entry = gatherEntries(rows.sortBy(_.path))._1
    entry.getOrElse(throw new EmptyInputException())

  val COMMAND_RE = "(?s)^\\s*[$] find (?<args>[^\r\n]+)[\r\n]+(?<body>.*)$".r

  def parseFullTranscript(transcript: String): (List[String], String) =
    transcript match
      case COMMAND_RE(args, body) => (args.split("\\s+").toList, body.replaceAll("(?m) +", ""))
      case _                      => throw new InvalidTranscriptFormatException()
