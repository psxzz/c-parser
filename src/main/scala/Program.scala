import scala.collection.mutable

class Environment {
    var variables = new mutable.HashMap[String, Int]
    var functions = new mutable.HashMap[String, Option[CompoundStmt]]

    override def toString: String = "Variables: " + variables.toString +
      "\n" + "Functions: " + functions.toString
};

case class Program() {
    var declarations = new mutable.Queue[Declaration]()


    def Execute = {
        val globalEnv : Environment = new Environment

        declarations.foreach(_.Execute(globalEnv))

        globalEnv.functions("main").get.Execute(globalEnv)

        println(globalEnv)
    }
}

trait Executable {
    def Execute(env: Environment): Unit
}

trait Calculable {
    def Calculate(env: Environment): Int
}