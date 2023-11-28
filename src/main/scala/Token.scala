import scala.collection.mutable.Map

sealed trait Token

// Program is a entry point for parser
case class Program() extends Token {
    // globalEnv is a global-scope environment that holds data about variables and functions
    val globalEnv : Map[String, Any] = Map[String, Any]()


    override def toString: String = globalEnv.toString()
}

case class Type(t : String) extends Token
case class Identifier(id : String) extends Token

case class IntValue(value : Int) extends Token
case class BoolValue(value : Boolean) extends Token

abstract class Variable extends Token
case class IntVariable(t : String, id : String, value : Int) extends Variable
case class BoolVariable(t : String, id : String, value : Boolean) extends Variable

abstract class Function(body : Option[CompoundStmt]) extends Token
case class IntFunction(id : String, body : Option[CompoundStmt]) extends Function(body)
case class BoolFunction(id : String, body : Option[CompoundStmt]) extends Function(body)

abstract class Statement extends Token {
    override def toString: String = "Stmt:"
}
case class CompoundStmt(var stmts : List[Statement]) extends Statement {
//    var stmts : List[Statement] = Nil

    override def toString: String = super.toString + "CompoundStmt: " + stmts.toString()
}
case class AssignStmt(dest : String, value : Any) extends Statement {
    override def toString: String = super.toString + "AssignStmt: " + dest + " -> " + value.toString
}