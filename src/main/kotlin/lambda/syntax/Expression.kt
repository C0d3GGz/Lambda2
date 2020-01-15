package lambda.syntax

import lambda.Scheme
import lambda.Span
import lambda.Spanned
import lambda.Type
import kotlin.math.exp

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
    data class Lambda(val binder: Name, val body: Expression, val sp: Span) : Expression() {
        fun foldArguments(): Pair<List<Name>, Expression> =
            when (body) {
                is Lambda -> {
                    val (args, newBody) = body.foldArguments()
                    (listOf(binder) + args) to newBody
                }
                else -> listOf(binder) to body
            }
    }

    data class App(val func: Expression, val arg: Expression, val sp: Span) : Expression()
    data class Typed(val expr: Expression, val type: Type, val sp: Span) : Expression() {
        override fun withSpan(span: Span) = Spanned(span, this)
    }

    data class Let(
        val recursive: Boolean,
        val binder: Name,
        val scheme: Scheme?,
        val expr: Expression,
        val body: Expression,
        val sp: Span
    ) :
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
    ) {
        fun freeVars(): HashSet<Name> = body.freeVars().also {
            it.removeAll(binders)
        }

        fun subst(name: Name, replacement: Expression): Case {
            return if (binders.contains(name)) this else {
                copy(body = body.subst(name, replacement))
            }
        }
    }

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

    fun freeVars(): HashSet<Name> {
        return when (this) {
            is Literal -> hashSetOf()
            is Var -> hashSetOf(name)
            is Lambda -> body.freeVars().also { it.remove(binder) }
            is App -> func.freeVars().also { it.addAll(arg.freeVars()) }
            is Typed -> expr.freeVars()
            is Let -> {
                if (recursive) {
                    expr.freeVars().also {
                        it.addAll(body.freeVars())
                        it.remove(binder)
                    }
                } else {
                    body.freeVars().also {
                        it.remove(binder)
                        it.addAll(expr.freeVars())
                    }
                }
            }
            is If -> condition.freeVars().also {
                it.addAll(thenBranch.freeVars())
                it.addAll(elseBranch.freeVars())
            }
            is Construction -> {
                val res = hashSetOf<Name>()
                exprs.forEach { res.addAll(it.freeVars()) }
                res
            }
            is Match -> expr.freeVars().also { res ->
                cases.forEach {
                    res.addAll(it.freeVars())
                }
            }
        }
    }

    fun subst(name: Name, replacement: Expression): Expression {
        // TODO: handle name capture
        return when (this) {
            is Literal -> this
            is Var -> if (name == name) replacement else this
            is Lambda -> {
                if (binder == name) this else copy(body = body.subst(name, replacement))
            }
            is App -> copy(arg = arg.subst(name, replacement), func = func.subst(name, replacement))
            is Typed -> copy(expr = expr.subst(name, replacement))
            is Let -> copy(
                expr = if (recursive && binder == name) expr else expr.subst(
                    name,
                    replacement
                ),
                body = if (binder == name) body else body.subst(name, replacement)
            )
            is If -> copy(
                condition = condition.subst(name, replacement),
                thenBranch = thenBranch.subst(name, replacement),
                elseBranch = elseBranch.subst(name, replacement)
            )
            is Construction -> copy(exprs = exprs.map { it.subst(name, replacement) })
            is Match -> copy(
                expr = expr.subst(name, replacement),
                cases = cases.map { it.subst(name, replacement) })
        }
    }

}
