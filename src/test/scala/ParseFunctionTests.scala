import org.scalatest.funsuite.AnyFunSuite

class ParseFunctionTests extends AnyFunSuite{
    val p = new CProgramParser

    test("New empty function") {
        var result = p.parse(p.funcDecl, "int foo();")
        assert(result.successful)
        assert(result.get == IntFunction("foo", None))

        result = p.parse(p.funcDecl, "bool bar();")
        assert(result.successful)
        assert(result.get == BoolFunction("bar", None))
    }

    test("New non-empty function") {
        val intWanted = IntFunction("foo", Some(CompoundStmt(AssignStmt("a", 5) :: Nil)))
        val boolWanted = BoolFunction("bar", Some(CompoundStmt(AssignStmt("a", 5) :: Nil)))

        var result = p.parse(p.funcDecl, "int foo() {" +
          "a = 5;" +
          "}")
        assert(result.successful)
        assert(result.get == intWanted)

        result = p.parse(p.funcDecl, "bool bar() {" +
          "a = 5;" +
          "}")
        assert(result.successful)
        assert(result.get == boolWanted)
    }

    // TODO: Add tests (wrong return type?)
}
