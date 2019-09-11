package lambda

import lambda.syntax.BoolLit
import lambda.syntax.Expression
import lambda.syntax.Lit
import lambda.syntax.Name

sealed class EvalExpression {
    data class Literal(val lit: Lit) : EvalExpression()
    data class Var(val name: Name) : EvalExpression()
    data class Lambda(val binder: Name, val body: EvalExpression) : EvalExpression()
    data class App(val func: EvalExpression, val arg: EvalExpression) : EvalExpression()
    data class Typed(val expr: EvalExpression, val type: Type) : EvalExpression()
    data class Let(val binder: Name, val expr: EvalExpression, val body: EvalExpression) : EvalExpression()
    data class If(val condition: EvalExpression, val thenBranch: EvalExpression, val elseBranch: EvalExpression) :
        EvalExpression()

    companion object {
        fun fromExpr(expr: Expression): EvalExpression = when (expr) {
            is Expression.Literal -> Literal(expr.lit)
            is Expression.Var -> Var(expr.name)
            is Expression.Lambda -> Lambda(expr.binder.value, fromExpr(expr.body.value))
            is Expression.App -> App(fromExpr(expr.func.value), fromExpr(expr.arg.value))
            is Expression.Typed -> Typed(fromExpr(expr.expr.value), expr.type.value)
            is Expression.Let -> Let(expr.binder.value, fromExpr(expr.expr.value), fromExpr(expr.body.value))
            is Expression.If -> If(
                fromExpr(expr.condition.value),
                fromExpr(expr.thenBranch.value),
                fromExpr(expr.elseBranch.value)
            )
            is Expression.Construction -> TODO()
            is Expression.Match -> TODO()
        }
    }
}

class Eval {

    var freshSupply = 0

    // substitute(s, r, e) = [s -> r] e
    fun substitute(scrutinee: Name, replacement: EvalExpression, expr: EvalExpression): EvalExpression {
        return when (expr) {
            is EvalExpression.Literal -> expr
            is EvalExpression.Var -> if (expr.name == scrutinee) replacement else expr
            is EvalExpression.Lambda ->
                when {
                    expr.binder == scrutinee -> expr
                    replacement.freeVars().contains(expr.binder) -> {
                        val freshBinder = freshName(expr.binder)
                        val renamedBody = substitute(expr.binder, EvalExpression.Var(freshBinder), expr.body)
                        EvalExpression.Lambda(freshBinder, substitute(scrutinee, replacement, renamedBody))
                    }
                    else -> EvalExpression.Lambda(expr.binder, substitute(scrutinee, replacement, expr.body))
                }
            is EvalExpression.App ->
                EvalExpression.App(
                    substitute(scrutinee, replacement, expr.func),
                    substitute(scrutinee, replacement, expr.arg)
                )
            is EvalExpression.Typed -> EvalExpression.Typed(substitute(scrutinee, replacement, expr.expr), expr.type)
            is EvalExpression.Let -> {
                val body = when {
                    expr.binder == scrutinee -> expr.body
                    replacement.freeVars().contains(expr.binder) -> {
                        val freshBinder = freshName(expr.binder)
                        val renamedBody = substitute(expr.binder, EvalExpression.Var(freshBinder), expr.body)
                        substitute(scrutinee, replacement, renamedBody)
                    }
                    else -> substitute(scrutinee, replacement, expr.body)
                }

                EvalExpression.Let(expr.binder, substitute(scrutinee, replacement, expr.expr), body)
            }
            is EvalExpression.If -> EvalExpression.If(
                substitute(scrutinee, replacement, expr.condition),
                substitute(scrutinee, replacement, expr.thenBranch),
                substitute(scrutinee, replacement, expr.elseBranch)
            )
        }
    }

    private fun freshName(oldName: Name): Name {
        freshSupply++
        return Name(freshSupply.toString() + oldName.value)
    }

    fun eval(expr: EvalExpression): EvalExpression {
        return when (expr) {
            is EvalExpression.App -> when (val evaledFunc = eval(expr.func)) {
                is EvalExpression.Lambda -> {
                    eval(substitute(evaledFunc.binder, eval(expr.arg), evaledFunc.body))
                }
                else -> EvalExpression.App(evaledFunc, expr.arg)
            }
            is EvalExpression.Let -> eval(substitute(expr.binder, expr.expr, expr.body))
            is EvalExpression.If -> {
                val evalCondition = eval(expr.condition)

                if (evalCondition is EvalExpression.Literal && evalCondition.lit is BoolLit) {
                    if (evalCondition.lit.bool) eval(expr.thenBranch) else eval(expr.elseBranch)
                } else {
                    EvalExpression.If(evalCondition, expr.thenBranch, expr.elseBranch)
                }
            }
            else -> expr
        }
    }
}

fun EvalExpression.freeVars(): Set<Name> {
    return when (this) {
        is EvalExpression.Literal -> emptySet()
        is EvalExpression.Var -> hashSetOf(name)
        is EvalExpression.Lambda -> body.freeVars().filter { it != binder }.toSet()
        is EvalExpression.App -> func.freeVars().union(arg.freeVars())
        is EvalExpression.Typed -> expr.freeVars()
        is EvalExpression.Let -> body.freeVars().filter { it != binder }.toSet().union(expr.freeVars())
        is EvalExpression.If -> condition.freeVars().union(thenBranch.freeVars()).union(elseBranch.freeVars())
    }
}
