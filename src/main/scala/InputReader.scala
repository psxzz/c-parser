abstract class InputSource {
    def getLines: String
}

class ConsoleInputSource extends InputSource {
    override def getLines: String = scala.io.Source.stdin.mkString
}

class FileInputSource(name : String) extends InputSource {
    override def getLines: String = {
        val f = scala.io.Source.fromFile(name)
        try f.mkString finally f.close()
    }
}


class InputReader(source: InputSource) {
    def read() : String = source.getLines
}

object InputReader {
    def apply(source: InputSource) = new InputReader(source)
}