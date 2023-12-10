import scala.collection.mutable

abstract class Statement {
    def Execute(env : Environment) : Unit
}

case class CompoundStmt() extends Statement {
    var declarations = mutable.Queue[Declaration]()
    var statements = mutable.Queue[Statement]()

    override def Execute(env: Environment): Unit = {
        declarations.foreach(_.Execute(env))

        statements.foreach(_.Execute(env))
    }
}

case class AssignStmt(id : String, exp : Expression) extends Statement {
    override def Execute(env: Environment): Unit = {
        env.variables(id) = exp.calc(env)
    }
}

case class PrintStmt(exp : Expression) extends Statement {
    override def Execute(env: Environment): Unit = {
        println(exp.calc(env))
    }
}