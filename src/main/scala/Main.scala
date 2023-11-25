import scala.util.parsing.combinator._

object Main {
    def main(args: Array[String]) = {
        val reader : InputReader = FileInputReader("./input/main.c")
//        val reader : InputReader = ConsoleInputReader()

        val lines = reader.read()

        println(lines)
    }
}