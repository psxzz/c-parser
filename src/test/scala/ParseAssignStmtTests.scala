import org.scalatest.funsuite.AnyFunSuite

class ParseAssignStmtTests extends AnyFunSuite {
    val p = new CProgramParser()
    test("Constant assignment") {
        var result = p.parse(p.assign, "a = 5;")
        assert(result.successful)
        assert(result.get == AssignStmt("a", 5))

        result = p.parse(p.assign, "b = true;")
        assert(result.successful)
        assert(result.get == AssignStmt("b", true))

        // TODO: add tests (unknown variable, wrong type value)
    }

    test("Variable assignment") {
        val result = p.parse(p.assign, "a = b;")
        assert(result.successful)
        assert(result.get == AssignStmt("a", Identifier("b")))

        // TODO: add tests (unknown dest variable, unknown source variable, wrong types)
    }
}
