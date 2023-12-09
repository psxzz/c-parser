object Main {
    def main(args: Array[String]): Unit = {
        val reader = InputReader(new FileInputSource("./input/main.c"))
//        val reader = InputReader(new ConsoleInputSource)
        val lines = reader.read()

        val parser : CProgramParser = new CProgramParser
        val result = parser.parse(parser.program, lines)

        result match {
            case parser.Success(matched, _) => println(matched)
            case parser.Failure(msg, _) => println("FAILURE: " + msg)
            case parser.Error(msg, _) => println("ERROR: " + msg)
        }

        result.get.Execute
    }
}