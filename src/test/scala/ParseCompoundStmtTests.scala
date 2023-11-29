import org.scalatest.funsuite.AnyFunSuite

class ParseCompoundStmtTests extends AnyFunSuite {
    val p = new CProgramParser()

    test("Empty compound statement") {
        val result = p.parse(p.compound, "{}")
        assert(result.successful)
        assert(result.get.stmts.isEmpty)
    }

    test("Non empty compound statement") {
        val result = p.parse(p.compound, "{ " +
          "a = 5;" +
          "}")
        assert(result.successful)
        assert(result.get.stmts.nonEmpty)
        assert(result.get.stmts.head == AssignStmt("a", 5))
    }
}
