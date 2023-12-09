import scala.util.parsing.combinator._

class CProgramParser extends RegexParsers {
    // Tokens
    val number = """\b(0|[1-9]\d*)\b""".r
    val identifier = """[a-zA-Z_][0-9a-zA-Z_]*""".r
    val typedef = """(int|bool)""".r

    def constant: Parser[Expression] = number ^^ {n => Number(n.toInt)}

    def varDecl: Parser[VarDecl] = typedef ~> identifier ~ opt("=" ~> constant) <~ ";" ^^ {
        case id ~ None => VarDecl(id, Number(0))
        case id ~ Some(v) => VarDecl(id, v)
    }

    def funcDecl: Parser[FuncDecl] = typedef ~> identifier <~ "()" <~ (";") ^^ {
        case id => FuncDecl(id)
    }

    def program: Parser[Program] = rep(varDecl | funcDecl) ^^ {
        case vars => {
            val p = Program()

            for (v <- vars) {
                p.declarations = v :: p.declarations
            }
            p
        }
    }
}
