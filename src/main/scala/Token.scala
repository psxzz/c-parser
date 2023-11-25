sealed trait Token

case class ProgramTk() extends Token {
    var globalVarDecls : List[VariableDeclTk] = Nil

    override def toString: String = globalVarDecls.toString()
}


case class VariableDeclTk(varType : TypeTk, id : IdentifierTk, value : String) extends Token

case class IdentifierTk(str : String) extends Token

case class TypeTk(t : String) extends Token

case class FunctionDeclTk(retType : TypeTk, id : IdentifierTk) extends Token