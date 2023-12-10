abstract class Expression {
    def calc(env : Environment) : Int
}

case class Number(value : Int) extends Expression {
    def calc(env : Environment) : Int = value
}

case class Identifier(id: String) extends Expression {
    def calc(env : Environment) : Int = env.variables(id)
}

case class Op(left: Expression, op: String, right: Expression) extends Expression {
    override def calc(env: Environment): Int = {
        op match {
            case "+" => left.calc(env) + right.calc(env)
            case "-" => left.calc(env) - right.calc(env)
            case "*" => left.calc(env) * right.calc(env)
            case "/" => left.calc(env) / right.calc(env)
        }
    }
}