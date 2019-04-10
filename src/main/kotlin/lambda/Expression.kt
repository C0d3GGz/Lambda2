package lambda

sealed class Expression
data class Var(val ident: Ident) : Expression()
data class Lamdba(val binder: Ident, val body: Expression) : Expression()
data class App(val func: Expression, val arg: Expression) : Expression()