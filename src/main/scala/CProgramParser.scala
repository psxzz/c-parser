import scala.util.parsing.combinator._

class CProgramParser extends RegexParsers {
    private val number = """\b(0|[1-9]\d*)\b""".r
    private val identifier = """[a-zA-Z_][0-9a-zA-Z_]*""".r
    private val typedef = """(int|bool)""".r

    // Expressions
    def expr : Parser[Expression] = arith

    def arith : Parser[Expression] = mult ~ rep(("+" | "-") ~ mult) ^^ {
        case e ~ Nil => e
        case e ~ r =>
            var result: Expression = e
            for (x <- r) {
                result = Op(result, x._1, x._2)
            }
            result
    }

    def mult : Parser[Expression] = constant ~ rep(("*" | "/") ~ constant) ^^ {
        case c ~ Nil => c
        case c ~ r =>
            var result : Expression = c
            for (x <- r) {
                result = Op(result, x._1, x._2)
            }
            result
    }

    def constant: Parser[Expression] = number ^^ {n => Number(n.toInt)} | identifier ^^ {id => Identifier(id)} | "(" ~> expr <~ ")"

    // Statements
    def statement : Parser[Statement] = compound | assign | print

    def compound : Parser[CompoundStmt] = "{" ~> rep(statement) <~ "}"^^ {
        case Nil => CompoundStmt()
        case stmts =>
            val comp = CompoundStmt()

            for (stmt <- stmts) {
                comp.statements += stmt
            }

            comp
    }

    def assign : Parser[AssignStmt] = identifier ~ "=" ~ expr <~ ";" ^^ {
        case id ~ _ ~ value => AssignStmt(id, value)
    }

    def print : Parser[PrintStmt] = "print" ~> "(" ~> expr <~ ")" ~ ";" ^^ (e => PrintStmt(e))


    // Declarations
    def varDecl: Parser[VarDecl] = typedef ~> identifier ~ opt("=" ~> expr) <~ ";" ^^ {
        case id ~ None => VarDecl(id, Number(0))
        case id ~ Some(v) => VarDecl(id, v)
    }

    def funcDecl: Parser[FuncDecl] = typedef ~> identifier ~ "()" ~ (compound | ";") ^^ {
        case id ~ _ ~ body => body match {
            case ";" => FuncDecl(id, None)
            case b : CompoundStmt => FuncDecl(id, Some(b))
        }
    }


    // Entrypoint
    def program: Parser[Program] = rep(varDecl | funcDecl) ^^ {
        decls => {
            val p = Program()

            for (d <- decls) {
                p.declarations += d
            }
            p
        }
    }
}

object CProgramParser {
    def apply() = new CProgramParser()
}