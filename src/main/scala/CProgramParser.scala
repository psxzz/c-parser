import scala.util.parsing.combinator._

class CProgramParser extends RegexParsers {
    // Tokens
    def identifier: Parser[Identifier] = """[a-zA-Z_][0-9a-zA-Z_]*""".r ^^ { id => Identifier(id.toString) }

    def number: Parser[IntValue] = """\b(0|[1-9]\d*)\b""".r ^^ { v => IntValue(v.toInt) }

    def bool: Parser[BoolValue] = """(true|false)""".r ^^ { v => BoolValue(v.toBoolean) }

    def typedef: Parser[Type] = """(int|bool)""".r ^^ { t => Type(t) }

    // Declarations
    def varDecl: Parser[Variable] = typedef ~ identifier ~ opt("=" ~> (bool | number)) <~ ";" ^^ { // TODO: replace (bool|number) with expression
        case t ~ id ~ Some(v) => v match {
            case IntValue(iv) => IntVariable(id.id, iv)    // TODO: handle wrong type error
            case BoolValue(bv) => BoolVariable(id.id, bv)  // TODO: handle wrong type error
        }
        case t ~ id ~ None => t.t match {
            case "int" => IntVariable(id.id, 0)
            case "bool" => BoolVariable(id.id, false)
        }
        // TODO: add default case?
    }

    def funcDecl: Parser[Function] = typedef ~ identifier ~ "()" ~ (compound | ";") ^^ {
        case t ~ id ~ _ ~ ";" => t.t match {
            case "int" => IntFunction(id.id, None)
            case "bool" => BoolFunction(id.id, None)
        }
        case t ~ id ~ _ ~ CompoundStmt(body)=> t.t match {
            case "int" => IntFunction(id.id, Some(CompoundStmt(body)))
            case "bool" => BoolFunction(id.id, Some(CompoundStmt(body)))
        }
    }

    // Statements
    def statement : Parser[Statement] = (compound | assign) ^^ {
        case AssignStmt(id, v) => AssignStmt(id, v)
    }

    def compound : Parser[CompoundStmt] = "{" ~> rep(statement) <~ "}" ^^ {
        stmts => {
            val res = CompoundStmt(Nil)
            for (stmt <- stmts) {
                res.stmts = stmt :: res.stmts
            }
            res
        }
    }

    def assign : Parser[AssignStmt] = identifier ~ "=" ~ (bool | number | identifier) <~ ";"^^ {
        case id  ~ _ ~ IntValue(v) => AssignStmt(id.id, v)
        case id  ~ _ ~ BoolValue(v) => AssignStmt(id.id, v)
        case id ~ _ ~ v  => AssignStmt(id.id, v)
    }

    // Entry point
    def program: Parser[Program] = rep(varDecl | funcDecl) ^^ {
        case vs =>
            val prog = Program()
            for (v <- vs) v match {
                case IntVariable(id, value) =>  prog.globalEnv += (id -> value)
                case BoolVariable(id, value) => prog.globalEnv += (id -> value)
                case IntFunction(id, body) => prog.globalEnv += (id -> body)
                case BoolFunction(id, body) => prog.globalEnv += (id -> body)
            }
            prog
    }
}
