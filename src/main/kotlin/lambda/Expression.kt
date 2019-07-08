package lambda

sealed class Lit
data class IntLit(val int: Int): Lit()
data class BoolLit(val bool: Boolean): Lit()

sealed class Expression {
    data class Literal(val lit: Lit) : Expression()
    data class Var(val ident: Ident) : Expression()
    data class Lambda(val binder: Ident, val body: Expression) : Expression()
    data class App(val func: Expression, val arg: Expression) : Expression()
    data class Typed(val expr: Expression, val type: Type): Expression()
}
