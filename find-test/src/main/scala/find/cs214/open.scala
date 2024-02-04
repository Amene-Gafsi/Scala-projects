package find.cs214

def open(path: String): Entry =
  val (root, base) =
    os.FilePath(path) match
      case rel: os.RelPath => (os.pwd / rel, Some(os.pwd))
      case rel: os.SubPath => (os.pwd / rel, Some(os.pwd))
      case abs: os.Path    => (abs, None)

  if !os.exists(root) then
    throw new java.nio.file.NoSuchFileException(path)

  new OSLibEntry(IndexedSeq(root), base, 0)
