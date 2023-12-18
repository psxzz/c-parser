import scala.collection.mutable

class Environment {
    var variables = new mutable.HashMap[String, Int]
    var functions = new mutable.HashMap[String, Option[CompoundStmt]]

    override def toString: String = "Variables: " + variables.toString +
      "\n" + "Functions: " + functions.toString
};

case class Program() {
    val globalEnv : Environment = new Environment
    var declarations = new mutable.Queue[Declaration]()


    def Execute(): Unit = {
        declarations.foreach(_.Execute(globalEnv))

        if (!globalEnv.functions.keySet.contains("main")) {
            println("[!] No function called \"main\" were found")
            return
        }

        globalEnv.functions("main").get.Execute(globalEnv)
        println("GLOBAL ENVIRONMENT\n" + globalEnv)
    }
}

trait Executable {
    def Execute(env: Environment): Unit
}

trait Calculable {
    def Calculate(env: Environment): Int
}