import org.scalatest.funsuite.AnyFunSuite

class ParseDeclarationsTests extends AnyFunSuite{
    val p: CProgramParser = CProgramParser()

    test("Parse variable declarations") {
        val env = new Environment

        var result = p.parse(p.varDecl, "int a = 5;")
        assert(result.successful)
        result.get.Execute(env)
        assert(env.variables("a") == 5)

        result = p.parse(p.varDecl, "int b;")
        assert(result.successful)
        result.get.Execute(env)
        assert(env.variables("b") == 0)
    }

    test("Parse function declarations") {
        val env = new Environment

        var result = p.parse(p.funcDecl, "int main();")
        assert(result.successful)

        result.get.Execute(env)
        assert(env.functions("main").isEmpty)

        result = p.parse(p.funcDecl, "int main() {}")
        result.get.Execute(env)
        assert(env.functions("main").isDefined)
    }
}
