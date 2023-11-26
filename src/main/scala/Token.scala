sealed trait Token

case class ProgramTk() extends Token {
    var globalVarDecls : List[VariableDeclTk] = Nil
    var globalFuncDecls : List[FunctionDeclTk] = Nil

    override def toString: String = globalVarDecls.toString() + "\n" + globalFuncDecls.toString()
}


abstract class Declaration extends Token

case class VariableDeclTk(varType : Type, id : Identifier, value : Option[Exp]) extends Declaration
case class FunctionDeclTk(retType : Type, id : Identifier, body : CompoundStmt) extends Declaration

case class Identifier(str : String) extends Token

case class Type(t : String) extends Token

case class IntTk(value : Int) extends Token
case class BoolTk(value : Boolean) extends Token


abstract class Exp extends Token
case class ArithExp(value : Int) extends Exp
case class BoolExp(value : Boolean) extends Exp
case class IdentifierExp(id: String) extends Exp

abstract class Stmt extends Token
case class CompoundStmt() extends Stmt
case class AssignStmt(id: Identifier, exp: Exp) extends Stmt

abstract class Op extends Token
case class AddOp(l :Int, r :Int) extends Op
case class SubOp(l :Int, r :Int) extends Op
case class MulOp(l :Int, r :Int) extends Op
case class DivOp(l :Int, r :Int) extends Op
