package lambda

private object Pretty{

    fun prettyPrintLiteral(lit: Lit): String {
        return when (lit) {
            is IntLit -> lit.int.toString()
        }
    }

    fun prettyPrintExpr(expr: Expression, depth: Int): String {
        return when(expr){
            is Literal -> prettyPrintLiteral(expr.lit)
            is Var -> expr.ident.ident
            is Lambda -> "(\\${expr.binder.ident}.${prettyPrintExpr(expr.body,0)})"
            is App -> {
                val output = "${prettyPrintExpr(expr.func, depth)} ${prettyPrintExpr(expr.arg, depth + 1)}"
                return if(depth > 0) "($output)" else output
            }
        }
    }

    fun prettyPrintRTExpr(expr: RTExpression, depth: Int): String {
        return when(expr){
            is RTLiteral -> prettyPrintLiteral(expr.lit)
            is RTVar -> expr.ident.ident
            is RTLambda -> "(\\${expr.binder.ident}.${prettyPrintRTExpr(expr.body,0)})"
            is RTClosure -> "(\\${expr.binder.ident}.${prettyPrintRTExpr(expr.body,0)})"
            is RTApp -> {
                val output = "${prettyPrintRTExpr(expr.func, depth)} ${prettyPrintRTExpr(expr.arg, depth + 1)}"
                return if(depth > 0) "($output)" else output
            }
        }
    }

}
fun Expression.pretty() = Pretty.prettyPrintExpr(this, 0)
fun RTExpression.pretty() = Pretty.prettyPrintRTExpr(this, 0)