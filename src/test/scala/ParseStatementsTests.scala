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
        assert(result.get.executables.isEmpty)

        result = p.parse(p.compound, "{" +
          "a = 5;" +
          "}")
        assert(result.successful)
        assert(result.get.executables.nonEmpty)
        assert(result.get.executables(0) == AssignStmt("a", Number(5)))

        result = p.parse(p.compound, "{" +
          "int a = 5;" +
          "}")
        assert(result.successful)
        assert(result.get.executables.nonEmpty)
        assert(result.get.executables(0) == VarDecl("a", Number(5)))
    }

    test("Parse loop statement") {
        val env = new Environment
        env.variables("a") = 0

        var result = p.parse(p.loop, "for 1 to 5 do a = a + 1;")
        assert(result.successful)

        result.get.Execute(env)
        assert(env.variables("a") == 5)

        env.variables("b") = 0
        result = p.parse(p.loop, "for 1 to 5 do {" +
          "a = a - 1;" +
          "b = b + 1;" +
          "}")
        assert(result.successful)

        result.get.Execute(env)
        assert(env.variables("a") == 0)
        assert(env.variables("b") == 5)
    }

    test("Parse return statement") {
        val env = new Environment

        var result = p.parse(p.ret, "return 0;")
        assert(result.successful)

        result.get.Execute(env)
        assert(env.variables("__return") == 0)

        env.variables("to_return") = 123
        result = p.parse(p.ret, "return to_return;")
        assert(result.successful)

        result.get.Execute(env)
        assert(env.variables("__return") == env.variables("to_return"))
    }
}
