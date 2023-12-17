import scala.util.parsing.combinator._

class CProgramParser extends RegexParsers {
    private val number = """\b(0|[1-9]\d*)\b""".r
    private val identifier = """[a-zA-Z_][0-9a-zA-Z_]*""".r
    private val typedef = """\b(int|bool)\b""".r

    // Expressions
    def expr : Parser[Expression] = call | arith

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

    def call : Parser[Expression] = identifier <~ "()" ^^ (id => Call(id))

    // Statements
    def statement : Parser[Statement] = compound | assign | print | loop | ret

    def compound : Parser[CompoundStmt] = "{" ~> rep(varDecl | funcDecl | statement) <~ "}"^^ {
        case Nil => CompoundStmt()
        case lines =>
            val comp = CompoundStmt()

            for (l <- lines) comp.declarationsAndStatements += l
            comp
    }

    def assign : Parser[AssignStmt] = identifier ~ "=" ~ expr <~ ";" ^^ {
        case id ~ _ ~ value => AssignStmt(id, value)
    }

    def print : Parser[PrintStmt] = "print" ~> "(" ~> expr <~ ")" ~ ";" ^^ (e => PrintStmt(e))

    def loop : Parser[LoopStmt] = "for" ~ expr ~ "to" ~ expr ~ "do" ~ statement ^^ {
      case _ ~ start ~ _ ~ stop ~ _ ~ stmt => LoopStmt(start, stop, stmt)
    }

    def ret : Parser[ReturnStmt] = "return" ~> expr <~ ";" ^^ (e => ReturnStmt(e))

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