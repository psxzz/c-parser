import scala.util.parsing.combinator._

class CProgramParser extends RegexParsers {
    def identifier : Parser[IdentifierTk] = """[a-zA-Z_][0-9a-zA-Z_]*""".r ^^ {id => IdentifierTk(id.toString)}
    def number : Parser[IntTk] = """(0|[1-9]\d*)""".r ^^ { v => IntTk(v.toInt) }
    def boolean : Parser[BoolTk] = """(true|false)""".r ^^ { v => BoolTk(v.toBoolean) }

    def typedef : Parser[TypeTk] = """(int|bool)""".r ^^ {
        t => TypeTk(t)
    }

    def varDecl : Parser[VariableDeclTk] = typedef ~ identifier ~ opt("=".r ~> (number | boolean)) <~ ";".r ^^ {
        case t ~ id ~ exp => VariableDeclTk(t, id, exp)
    }

    def program : Parser[ProgramTk] = rep(varDecl) ^^ {
        case vs => {
            val prog = ProgramTk()
            for (v <- vs) {
                prog.globalVarDecls = v :: prog.globalVarDecls
            }
            prog
        }
    }
}
