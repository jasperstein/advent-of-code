import scala.io.Source
object Day7 extends App {
    val input = Source.fromFile("inputs/day7.txt").getLines().map(_.split(" ").toList)

    case class File(name: String, size: Long)
    case class Dir(name: String, dirs: Map[String, Dir], files: Map[String, File], totalSize: Long)

    case class Fs(pwd: List[String], dir: Dir)

    val emptyFs = Fs(List(), Dir("/", Map(), Map(), 0))

    def moveUp(fs: Fs) = fs.copy(pwd = fs.pwd.tail)
    def cd(fs: Fs, dirName: String) = fs.copy(pwd = dirName +: fs.pwd)
    def registerFile(at: Dir, name: String, size: Long): Dir = at.copy(files = at.files + (name -> File(name, size)), totalSize = at.totalSize + size)
    def registerDir(at: Dir, name: String) = at.copy(dirs = at.dirs + (name -> at.dirs.get(name).getOrElse(Dir(name, Map(), Map(), 0))))
    def registerFile(fs: Fs, name: String, size: Long): Fs = 
        if (fs.pwd.isEmpty) fs.copy(dir = registerFile(fs.dir, name, size))
        else fs.copy(dir = fs.dir.copy(dirs = fs.dir.dirs + (fs.pwd.last -> registerFile(Fs(fs.pwd.init, fs.dir.dirs(fs.pwd.last)), name, size).dir), totalSize = fs.dir.totalSize + size))
    def registerDir(fs: Fs, name: String): Fs = 
        if (fs.pwd.isEmpty) fs.copy(dir = registerDir(fs.dir, name))
        else fs.copy(dir = fs.dir.copy(dirs = fs.dir.dirs + (fs.pwd.last -> registerDir(Fs(fs.pwd.init, fs.dir.dirs(fs.pwd.last)), name).dir)))

    // println(moveUp(Fs(List("sub2", "sub1", "etc"), Dir("/", Map(), Map(), 0))))
    // println(cd(Fs(List("sub2", "sub1", "etc"), Dir("/", Map(), Map(), 0)), "sub3"))
    // println(registerFile(Dir("/", Map(), Map(), 0), "file", 1234L))
    // println(registerDir(Dir("/", Map(), Map(), 0), "etc"))
    // println(registerFile(Fs(List("local", "usr"), Dir("/", Map("usr" -> Dir("usr", Map("local" -> Dir("local", Map(), Map("file1" -> File("file1", 1234)), 1234)), Map(), 1234)), Map(), 1234)), "file2", 4321))
    // println(registerDir(Fs(List("local", "usr"), Dir("/", Map("usr" -> Dir("usr", Map("local" -> Dir("local", Map("bin" -> Dir("bin", Map(), Map(), 0)), Map("file1" -> File("file1", 1234)), 1234)), Map(), 1234)), Map(), 1234)), "bin2"))

    val fs = input.foldLeft(emptyFs) { 
        case (fs, "$" :: "cd" :: "/" :: Nil) => fs
        case (fs, "$" :: "cd" :: ".." :: Nil) => moveUp(fs)
        case (fs, "$" :: "cd" :: name :: Nil) => cd(fs, name)
        case (fs, "$" :: "ls" :: Nil) => fs
        case (fs, "dir" :: name :: Nil) => registerDir(fs, name)
        case (fs, size :: name :: Nil) => registerFile(fs, name, java.lang.Long.parseLong(size))
        case (fs, args) => { println(s"wut @ $args"); fs}
    }
    
    def addSizes(dir: Dir): Long = (if (dir.totalSize > 100_000) 0 else dir.totalSize) + dir.dirs.values.map(addSizes).sum
    println(addSizes(fs.dir))

    val free = 70_000_000 - fs.dir.totalSize
    val reqd = 30_000_000 - free

    def allSizes(dir: Dir): Seq[Long] = dir.totalSize +: dir.dirs.values.toSeq.flatMap(allSizes)
    println(allSizes(fs.dir).sorted.dropWhile(_ < reqd).head)
}
