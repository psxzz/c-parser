import scala.collection.mutable

abstract class Statement extends Executable

case class CompoundStmt() extends Statement {
    var declarationsAndStatements = mutable.Queue[Executable]()

    override def Execute(env: Environment): Unit = {
        declarationsAndStatements.foreach(_.Execute(env))
    }
}

case class AssignStmt(id : String, exp : Expression) extends Statement {
    override def Execute(env: Environment): Unit = {
        env.variables(id) = exp.Calculate(env)
    }
}

case class PrintStmt(exp : Expression) extends Statement {
    override def Execute(env: Environment): Unit = {
        println(exp.Calculate(env))
    }
}

case class LoopStmt(start : Expression, stop : Expression, stmt: Statement) extends Statement {
    override def Execute(env: Environment): Unit = {
        for (_ <- start.Calculate(env) to stop.Calculate(env)) stmt.Execute(env)
    }
}

case class ReturnStmt(exp : Expression) extends Statement {
    override def Execute(env: Environment): Unit = env.variables("__return") = exp.Calculate(env)
}