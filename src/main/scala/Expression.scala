abstract class Expression extends Calculable

case class Number(value : Int) extends Expression {
    override def Calculate(env: Environment): Int = value
}

case class Identifier(id: String) extends Expression {
    override def Calculate(env: Environment): Int = env.variables(id)
}

case class Op(left: Expression, op: String, right: Expression) extends Expression {
    override def Calculate(env: Environment): Int = {
        op match {
            case "+" => left.Calculate(env) + right.Calculate(env)
            case "-" => left.Calculate(env) - right.Calculate(env)
            case "*" => left.Calculate(env) * right.Calculate(env)
            case "/" => left.Calculate(env) / right.Calculate(env)
        }
    }
}

case class Call(id : String) extends Expression {
    override def Calculate(env: Environment): Int = {
        env.functions(id).get.Execute(env)
        env.variables("__return")
    }
}