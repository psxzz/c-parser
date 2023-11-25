object Main {
    def main(args: Array[String]) = {
        val reader : InputReader = FileInputReader("./input/main.c")
//        val reader : InputReader = ConsoleInputReader()
        val lines = reader.read()

        val parser : CProgramParser = new CProgramParser
        val result = parser.parse(parser.program, lines)

        result match {
            case parser.Success(matched, _) => println(matched)
            case parser.Failure(msg, _) => println("FAILURE: " + msg)
            case parser.Error(msg, _) => println("ERROR: " + msg)
        }
    }
}