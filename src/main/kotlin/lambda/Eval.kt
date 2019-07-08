package lambda

class Eval {

    var freshSupply = 0

    // substitute(s, r, e) = [s -> r] e
    fun substitute(scrutinee: Ident, replacement: Expression, expr: Expression): Expression {
        return when (expr) {
            is Expression.Literal -> expr
            is Expression.Var -> if (expr.ident == scrutinee) replacement else expr
            is Expression.Lambda ->
                when {
                    expr.binder == scrutinee -> expr
                    replacement.freeVars().contains(expr.binder) -> {
                        val freshBinder = freshName(expr.binder)
                        val renamedBody = substitute(expr.binder, Expression.Var(freshBinder), expr.body)
                        Expression.Lambda(freshBinder, substitute(scrutinee, replacement, renamedBody))
                    }
                    else -> Expression.Lambda(expr.binder, substitute(scrutinee, replacement, expr.body))
                }
            is Expression.App ->
                Expression.App(
                    substitute(scrutinee, replacement, expr.func),
                    substitute(scrutinee, replacement, expr.arg)
                )
            is Expression.Typed -> Expression.Typed(substitute(scrutinee, replacement, expr.expr), expr.type)
        }
    }

    private fun freshName(oldName: Ident): Ident {
        freshSupply++
        return Ident(freshSupply.toString() + oldName.ident)
    }

    fun eval(expr: Expression): Expression {
        return when (expr) {
            is Expression.App -> when (val evaledFunc = eval(expr.func)) {
                is Expression.Lambda -> {
                    eval(substitute(evaledFunc.binder, eval(expr.arg), evaledFunc.body))
                }
                else -> Expression.App(evaledFunc, expr.arg)
            }
            else -> expr
        }
    }
}

fun Expression.freeVars(): Set<Ident> {
    return when (this) {
        is Expression.Literal -> emptySet()
        is Expression.Var -> hashSetOf(ident)
        is Expression.Lambda -> body.freeVars().filter { it != binder }.toSet()
        is Expression.App -> func.freeVars().union(arg.freeVars())
        is Expression.Typed -> expr.freeVars()
    }
}
