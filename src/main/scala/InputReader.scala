abstract class InputReader {
    def read(): String
}

class FileInputReader(filename: String) extends InputReader {
    override def read(): String = {
        val source = scala.io.Source.fromFile(filename)
        val lines = try source.mkString finally source.close()

        lines
    }
}

object FileInputReader {
    def apply(filename: String) = new FileInputReader(filename)
}


class ConsoleInputReader extends InputReader {
    override def read(): String = {
        val source = scala.io.Source.stdin
        val lines = try source.mkString finally source.close()

        lines
    }
}

object ConsoleInputReader {
    def apply() = new ConsoleInputReader
}