package lambda

object Eval {

    fun substitute(scrutinee: Ident, replacement: Expression, expr: Expression): Expression{
        return when(expr){
            is Var -> if(expr.ident == scrutinee) replacement else expr
            is Lamdba -> if(expr.binder != scrutinee) Lamdba(expr.binder, substitute(scrutinee, replacement, expr.body)) else expr
            is App -> App(substitute(scrutinee, replacement, expr.func), substitute(scrutinee, replacement, expr.arg))
        }
    }
}

fun Expression.freeVars(): Set<Ident>{
    return when(this){
        is Var -> hashSetOf(ident)
        is Lamdba -> body.freeVars().filter { it != binder }.toSet()
        is App -> func.freeVars().union(arg.freeVars())
    }
}
