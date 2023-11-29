import org.scalatest.funsuite.AnyFunSuite

class ParseVariableTests extends AnyFunSuite{
    val p = new CProgramParser()

    test("New variable with default value") {
        var result = p.parse(p.varDecl, "int i;")
        assert(result.successful)
        assert(result.get == IntVariable("i", 0))

        result = p.parse(p.varDecl, "bool b;")
        assert(result.successful)
        assert(result.get == BoolVariable("b", false))
    }

    test("New variable with assignment") {
        var result = p.parse(p.varDecl, "int i = 10;")
        assert(result.successful)
        assert(result.get == IntVariable("i", 10))

        result = p.parse(p.varDecl, "bool b = true;")
        assert(result.successful)
        assert(result.get == BoolVariable("b", true))
    }
}
