import org.scalatest.funsuite.AnyFunSuite

class InputReaderTests extends AnyFunSuite {
    test("InputReader reads from file") {
        val result = new FileInputReader("./src/test/samples/simple_input.c").read()
        val expected = s"int main() {\r\n    printf(\"Hello world!\");\r\n    return 0;\r\n}"
        assert(result == expected)
    }
}
