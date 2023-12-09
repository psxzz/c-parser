abstract class Declaration {
    def Execute(env : Environment): Unit
}

case class VarDecl(id: String, exp: Expression) extends Declaration {
    override def Execute(env : Environment): Unit = {
        env.variables += (id -> exp.calc(env))
    }
}

case class FuncDecl(id: String, body: Option[CompoundStmt]) extends Declaration {
    override def Execute(env: Environment): Unit = {
        env.functions += (id -> body)
    }
}