import scala.util.parsing.combinator._

class CProgramParser extends RegexParsers {
    // Tokens
    def identifier: Parser[Identifier] = """[a-zA-Z_][0-9a-zA-Z_]*""".r ^^ { id => Identifier(id.toString) }

    def number: Parser[IntTk] = """(0|[1-9]\d*)""".r ^^ { v => IntTk(v.toInt) }

    def bool: Parser[BoolTk] = """(true|false)""".r ^^ { v => BoolTk(v.toBoolean) }

    def typedef: Parser[Type] = """(int|bool)""".r ^^ { t => Type(t) }

    // Declarations
    def varDecl: Parser[VariableDeclTk] = typedef ~ identifier ~ opt("=" ~> (arithExp | boolExp)) <~ ";" ^^ {
        case t ~ id ~ exp => VariableDeclTk(t, id, exp)
    }

    def funcDecl: Parser[FunctionDeclTk] = typedef ~ identifier ~ "()" ~ compound ^^ {
        case t ~ id ~ _ ~ body => FunctionDeclTk(t, id, body)
    }

    // Statements
    def statement: Parser[Stmt] = compound | assign ^^ { _ => CompoundStmt() }

    def compound: Parser[CompoundStmt] = "{" ~ rep(statement) ~ "}" ^^ { _ => CompoundStmt() }

    def assign: Parser[AssignStmt] = identifier ~ "=" ~ expression ^^ {
        case id ~ _ ~ exp => AssignStmt(id, exp)
    }

    // Expressions
    def expression: Parser[Exp] = identifierExp | constantExp | arithExp | boolExp ^^ {}

    def identifierExp: Parser[IdentifierExp] = identifier ^^ {}

    def constantExp: Parser[ConstantExp] = number | bool ^^ {}

    def arithExp: Parser[ArithExp] = add | sub ^^ {}

    def boolExp: Parser[BoolExp] = bool ^^ {}

    // Arithmetical operations
    def add: Parser[AddOp] = expression ~ "+" ~ expression ^^ {}

    def sub: Parser[SubOp] = expression ~ "-" ~ expression ^^ {}
    // Boolean operations

    // Entry point
    def program: Parser[ProgramTk] = rep(varDecl | funcDecl) ^^ {
        case vs =>
            val prog = ProgramTk()
            for (v <- vs) v match {
                case VariableDeclTk(t, id, value) => {
                    prog.globalVarDecls = VariableDeclTk(t, id, value) :: prog.globalVarDecls
                }
                case FunctionDeclTk(t, id, body) => {
                    prog.globalFuncDecls = FunctionDeclTk(t, id, body) :: prog.globalFuncDecls
                }
            }
            prog
    }
}
