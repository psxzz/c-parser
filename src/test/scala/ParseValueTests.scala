import org.scalatest.funsuite.AnyFunSuite

class ParseValueTests extends AnyFunSuite{
    val p = new CProgramParser()

    test("Correct values") {
        val intResult = p.parse(p.number, "123")
        assert(intResult.successful)
        assert(intResult.get == IntValue(123))

        val boolResult = p.parse(p.bool, "true")
        assert(boolResult.successful)
        assert(boolResult.get == BoolValue(true))
    }

    test("Incorrect values") {
        var intResult = p.parse(p.number, "12d3")
        assert(!intResult.successful)

        intResult = p.parse(p.number, "043")
        assert(!intResult.successful)

        val boolResult = p.parse(p.bool, "falce")
        assert(!boolResult.successful)
    }

}
