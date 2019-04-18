package lambda

class Eval {

    var freshSupply = 0

    // substitute(s, r, e) = [s -> r] e
    fun substitute(scrutinee: Ident, replacement: Expression, expr: Expression): Expression {
        return when (expr) {
            is Var -> if (expr.ident == scrutinee) replacement else expr
            is Lamdba ->
                when {
                    expr.binder == scrutinee -> expr
                    replacement.freeVars().contains(expr.binder) -> {
                        val freshBinder = freshName(expr.binder)
                        val renamedBody = substitute(expr.binder, Var(freshBinder), expr.body)
                        Lamdba(freshBinder, substitute(scrutinee, replacement, renamedBody))
                    }
                    else -> Lamdba(expr.binder, substitute(scrutinee, replacement, expr.body))
                }
            is App -> App(substitute(scrutinee, replacement, expr.func), substitute(scrutinee, replacement, expr.arg))
        }
    }

    private fun freshName(oldName: Ident): Ident {
        freshSupply++
        return Ident(freshSupply.toString() + oldName.ident)
    }

    fun eval(expr: Expression): Expression {
        return when(expr) {
            is App -> when (val evaledFunc = eval(expr.func)) {
                is Lamdba -> {
                    eval(substitute(evaledFunc.binder, eval(expr.arg), evaledFunc.body))
                }
                else -> App(evaledFunc, expr.arg)
            }
            else -> expr
        }
    }
}

fun Expression.freeVars(): Set<Ident> {
    return when (this) {
        is Var -> hashSetOf(ident)
        is Lamdba -> body.freeVars().filter { it != binder }.toSet()
        is App -> func.freeVars().union(arg.freeVars())
    }
}
