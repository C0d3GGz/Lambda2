package lambda

sealed class Lit
data class IntLit(val int: Int): Lit()

sealed class Expression
data class Literal(val lit: Lit) : Expression()
data class Var(val ident: Ident) : Expression()
data class Lambda(val binder: Ident, val body: Expression) : Expression()
data class App(val func: Expression, val arg: Expression) : Expression()