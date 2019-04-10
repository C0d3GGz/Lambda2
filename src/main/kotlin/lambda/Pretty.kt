package lambda

private object Pretty{

    fun prettyPrintInner(expr: Expression, depth: Int): String {
        return when(expr){
            is Var -> expr.ident.ident
            is Lamdba -> "(\\${expr.binder.ident}.${prettyPrintInner(expr.body,0)})"
            is App -> {
                val output = "${prettyPrintInner(expr.func, depth)} ${prettyPrintInner(expr.arg, depth + 1)}"
                return if(depth > 0) "($output)" else output
            }
        }
    }

}
fun Expression.pretty() = Pretty.prettyPrintInner(this, 0)