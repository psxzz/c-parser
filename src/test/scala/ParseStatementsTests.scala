import org.scalatest.funsuite.AnyFunSuite

class ParseStatementsTests extends AnyFunSuite {
    val p: CProgramParser = CProgramParser()

    test("Parse print statement") {
        val result = p.parse(p.print, "print(123);")
        assert(result.successful)
    }

    test("Parse assign statement") {
        val env = new Environment
        val result = p.parse(p.assign, "a = 5;")
        assert(result.successful)

        env.variables += ("a" -> 0)
        result.get.Execute(env)

        assert(env.variables("a") == 5)
    }

    test("Parse compound statement") {
        var result = p.parse(p.compound, "{}")
        assert(result.successful)
        assert(result.get.statements.isEmpty)

        result = p.parse(p.compound, "{" +
          "a = 5;" +
          "}")
        assert(result.successful)
        assert(result.get.statements.nonEmpty)
        assert(result.get.statements(0) == AssignStmt("a", Number(5)))
    }


}
