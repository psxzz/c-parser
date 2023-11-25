sealed trait Token

case class ProgramTk() extends Token {
    var globalVarDecls : List[VariableDeclTk] = Nil

    override def toString: String = globalVarDecls.toString()
}


case class VariableDeclTk(varType : TypeTk, id : IdentifierTk, value : Option[Exp]) extends Token

case class IdentifierTk(str : String) extends Token

case class TypeTk(t : String) extends Token

case class FunctionDeclTk(retType : TypeTk, id : IdentifierTk) extends Token

abstract class Exp extends Token
case class IntTk(value : Int) extends Exp
case class BoolTk(value : Boolean) extends Exp

