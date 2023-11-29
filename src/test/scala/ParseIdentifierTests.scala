import org.scalatest.funsuite.AnyFunSuite

class ParseIdentifierTests extends AnyFunSuite{
    val p = new CProgramParser()

    test("Correct identifiers") {
        var result = p.parse(p.identifier, "hello_world123")
        assert(result.successful)

        result = p.parse(p.identifier, "_foo1")
        assert(result.successful)

        result = p.parse(p.identifier, "_foo1BAR2_Buzz3_")
        assert(result.successful)

        result = p.parse(p.identifier, "_123")
        assert(result.successful)
    }

    test("Incorrect identifiers") {
        var result = p.parse(p.identifier, "1foo_")
        assert(!result.successful)

        result = p.parse(p.identifier, "123")
        assert(!result.successful)
    }
}
