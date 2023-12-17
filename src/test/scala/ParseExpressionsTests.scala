import org.scalatest.funsuite.AnyFunSuite

class ParseExpressionsTests extends AnyFunSuite{
    val p: CProgramParser = CProgramParser()

    test("Parse constant") {
        val env = new Environment

        var result = p.parse(p.constant, "123")
        assert(result.successful)
        assert(result.get.Calculate(env) == 123)

        env.variables += ("a" -> 5)
        result = p.parse(p.constant, "a")
        assert(result.successful)
        assert(result.get.Calculate(env) == 5)

        result = p.parse(p.constant, "(1 + 2)")
        assert(result.successful)
        assert(result.get.Calculate(env) == 3)
    }

    test("Parse mult") {
        val env = new Environment

        var result = p.parse(p.mult, "2 * 3")
        assert(result.successful)
        assert(result.get.Calculate(env) == 6)

        result = p.parse(p.mult, "2 * 3 * 4 * 5")
        assert(result.successful)
        assert(result.get.Calculate(env) == 120)

        result = p.parse(p.mult, "2 / 2")
        assert(result.successful)
        assert(result.get.Calculate(env) == 1)

        result = p.parse(p.mult, "4 * 3 / 2")
        assert(result.successful)
        assert(result.get.Calculate(env) == 6)

        env.variables += ("a" -> 4)

        result = p.parse(p.mult, "a * 3 / 2")
        assert(result.successful)
        assert(result.get.Calculate(env) == 6)
    }

    test("Parse arith") {
        val env = new Environment

        var result = p.parse(p.arith, "4 * 2 + 4 * 4 / 2")
        assert(result.successful)
        assert(result.get.Calculate(env) == 16)

        result = p.parse(p.arith, "4 * (2 + 3) / 2")
        assert(result.successful)
        assert(result.get.Calculate(env) == 10)

        env.variables += ("a" -> 5)
        result = p.parse(p.arith, "a * (6 / 2) * 2")
        assert(result.successful)
        assert(result.get.Calculate(env) == 30)
    }

}
