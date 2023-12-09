abstract class Expression {
    def calc(env : Environment) : Int
}

case class Number(value : Int) extends Expression {
    def calc(env : Environment) : Int = value
}

