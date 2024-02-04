package anagrams.cli

import scala.io.Source
import scala.io.Codec
import anagrams.Anagrams.*

def wordsAnagrams(dict: Dictionary, words: List[String]) =
  words.map(wordAnagrams(dict, _))

case class CLI(
    words: List[String],
    wordlist: String,
    findAnagrams: (Dictionary, List[String]) => List[List[String]]
)

val CLI_DEFAULT = CLI(
  words = List(),
  wordlist = "en-ngsl",
  findAnagrams = sentenceAnagrams
)

def parseOpts(args: Seq[String]): CLI =
  def loop(args: Seq[String], cli: CLI): CLI =
    args match
      case Seq() => cli
      case Seq("--wordlist", name, rest*) =>
        loop(rest, cli.copy(wordlist = name))
      case Seq("--one-by-one", rest*) =>
        loop(rest, cli.copy(findAnagrams = wordsAnagrams))
      case Seq(word, rest*) =>
        if word.startsWith("-") then
          throw new IllegalArgumentException(f"Unrecognized argument $word.")
        loop(rest, cli.copy(words = word :: cli.words))
  val cli = loop(args, CLI_DEFAULT)
  if cli.words.length == 0 then
    throw new IllegalArgumentException(f"Need at least one word.")
  cli.copy(words = cli.words.reverse)

@main def main(args: String*) =
  try
    val cli = parseOpts(args)
    val words = cli.words.map(normalizeString)
    val dict = createDictionary(loadWordlist(cli.wordlist))
    cli.findAnagrams(dict, words).foreach(ws => println(ws.mkString(" ")))
  catch
    case e: IllegalArgumentException =>
      System.err.println(e.getMessage())
      System.err.println("Usage: run [--one-by-one] [--wordlist name] word [word…]")
    case e: Exception =>
      System.err.println(e.getMessage())

def loadWordlist(name: String): List[String] =
  val fname = f"$name.txt"
  val wordstream =
    Option(this.getClass.getResourceAsStream(f"/anagrams/$fname"))
      .getOrElse(sys.error(f"File not found: $fname"))
  try Source.fromInputStream(wordstream)(Codec.UTF8).getLines().toList
  finally wordstream.close()
