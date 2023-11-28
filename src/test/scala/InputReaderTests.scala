import org.scalatest.funsuite.AnyFunSuite

class InputReaderTests extends AnyFunSuite {
    test("InputReader reads from file") {
        val result = new FileInputReader("./src/test/samples/simple_input.c").read().replace("\r\n", "\n")
        val expected = s"int main() {\n    printf(\"Hello world!\");\n    return 0;\n}"
        assert(result == expected)
    }
}
