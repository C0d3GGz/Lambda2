package lambda.syntax

import lambda.Span
import lambda.Spanned
import lambda.Type

sealed class Lit {
    val span: Span
        get() = when (this) {
            is Int -> sp
            is Bool -> sp
            is String -> sp
        }

    data class Int(val int: kotlin.Int, val sp: Span = Span.DUMMY) : Lit()
    data class Bool(val bool: Boolean, val sp: Span = Span.DUMMY) : Lit()
    data class String(val string: kotlin.String, val sp: Span = Span.DUMMY) : Lit()
}

sealed class Expression {
    data class Literal(val lit: Lit) : Expression()
    data class Var(val name: Name) : Expression()
    data class Lambda(val binder: Name, val body: Expression, val sp: Span) : Expression()
    data class App(val func: Expression, val arg: Expression, val sp: Span) : Expression()
    data class Typed(val expr: Expression, val type: Type, val sp: Span) : Expression() {
        override fun withSpan(span: Span) = Spanned(span, this)
    }

    data class Let(val recursive: Boolean, val binder: Name, val expr: Expression, val body: Expression, val sp: Span) :
        Expression()

    data class If(
        val condition: Expression,
        val thenBranch: Expression,
        val elseBranch: Expression,
        val sp: Span
    ) : Expression()

    data class Construction(val type: Name, val dtor: Name, val exprs: List<Expression>, val sp: Span) :
        Expression()

    data class Match(val expr: Expression, val cases: List<Case>, val sp: Span) : Expression()

    data class Case(
        val type: Name,
        val dtor: Name,
        val binders: List<Name>,
        val body: Expression,
        val span: Span
    )

    val span: Span
        get() = when (this) {
            is Literal -> lit.span
            is Var -> name.span
            is Lambda -> sp
            is App -> sp
            is Typed -> sp
            is Let -> sp
            is If -> sp
            is Construction -> sp
            is Match -> sp
        }

    open fun withSpan(span: Span) = Spanned(span, this)

}
