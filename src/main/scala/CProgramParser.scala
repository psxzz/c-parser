import scala.util.parsing.combinator._

class CProgramParser extends RegexParsers {
    private val number = """\b(0|[1-9]\d*)\b""".r
    private val identifier = """[a-zA-Z_][0-9a-zA-Z_]*""".r
    private val typedef = """(int|bool)""".r

    def expr : Parser[Expression] = constant | identifier ^^ {id => Identifier(id)}

    def constant: Parser[Expression] = number ^^ {n => Number(n.toInt)}

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

    def print : Parser[PrintStmt] = "print" ~> "(" ~> expr <~ ")" ^^ (e => PrintStmt(e))

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