import scala.collection.mutable

class Environment extends mutable.HashMap[String, Int];

case class Program() {
    var declarations = List[Declaration]()


    def Execute = {
        var globalVarEnv : Environment = new Environment
        var globalFuncEnv : Environment = new Environment

        declarations.foreach {
            case v: VarDecl => v.Execute(globalVarEnv)
            case f: FuncDecl => f.Execute(globalFuncEnv)
        }

        println("Variables: " + globalVarEnv.toString)
        println("Functions: " + globalFuncEnv.toString)
    }
}

abstract class Declaration {
    def Execute(env : Environment): Unit
}

case class VarDecl(id: String, exp: Expression) extends Declaration {
    override def Execute(env : Environment): Unit = {
        env += (id -> exp.calc(env))
    }
}

case class FuncDecl(id: String) extends Declaration {
    override def Execute(env: Environment): Unit = {
        env += (id -> -1)
    }
}


//case class Func(identifier: String, value: Int) extends Declaration;
