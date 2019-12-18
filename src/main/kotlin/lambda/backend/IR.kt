package lambda.backend

import lambda.syntax.Name

sealed class Lit {
    data class Int(val int: kotlin.Int) : Lit()
    data class Bool(val bool: Boolean) : Lit()
    data class String(val string: kotlin.String) : Lit()

    companion object {
        fun fromSyntax(lit: lambda.syntax.Lit): Lit =
            when (lit) {
                is lambda.syntax.Lit.Int -> Int(lit.int)
                is lambda.syntax.Lit.Bool -> Bool(lit.bool)
                is lambda.syntax.Lit.String -> String(lit.string)
            }
    }

}

sealed class Expression {
    data class Literal(val lit: Lit) : Expression()
    data class Var(val name: LnName) : Expression()
    data class App(val func: Expression, val arg: Expression) : Expression()

    data class Let(val binder: Name, val expr: Expression, val body: Expression) : Expression()

    data class If(
        val condition: Expression,
        val thenBranch: Expression,
        val elseBranch: Expression
    ) : Expression()

    data class Pack(val tag: Int, val exprs: List<Expression>) : Expression()

    data class Match(val expr: Expression, val cases: List<Case>) : Expression()

    data class Case(
        val tag: Int,
        val binders: List<Name>,
        val body: Expression
    ) {
        fun instantiate(depth: Int, replacements: List<Expression>): Case {
            return Case(tag, binders, body.instantiateInner(depth + 1, replacements))
        }
    }

    fun instantiate(replacements: List<Expression>): Expression {
        return instantiateInner(0, replacements)
    }

    fun instantiateInner(depth: Int, replacements: List<Expression>): Expression {
        return when (this) {
            is Literal -> this
            is Var ->
                when {
                    name is LnName.Bound && name.index.depth == depth -> {
                        replacements[name.index.width]
                    }
                    else -> this
                }
            is App -> App(
                func.instantiateInner(depth, replacements),
                arg.instantiateInner(depth, replacements)
            )
            is Let -> Let(
                binder,
                expr.instantiateInner(depth, replacements),
                body.instantiateInner(depth + 1, replacements)
            )
            is If -> If(
                condition.instantiateInner(depth, replacements),
                thenBranch.instantiateInner(depth, replacements),
                elseBranch.instantiateInner(depth, replacements)
            )
            is Pack -> Pack(
                tag, exprs.map { it.instantiateInner(depth, replacements) }
            )
            is Match -> Match(
                expr.instantiateInner(depth, replacements),
                cases.map { it.instantiate(depth, replacements) }
            )
        }
    }
}

data class Declaration(val name: Name, val args: List<Name>, val body: Expression)

sealed class LnName {
    data class Bound(val index: Index) : LnName()
    data class Free(val name: Name) : LnName()

    data class Index(val depth: Int, val width: Int) {
        fun shift(depth: Int = 1) = this.copy(depth = this.depth + depth)
    }
}