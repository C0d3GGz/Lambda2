package lambda

import io.vavr.kotlin.*

private object Pretty{

    fun prettyPrintLiteral(lit: Lit): String {
        return when (lit) {
            is IntLit -> lit.int.toString()
            is BoolLit -> lit.bool.toString()
        }
    }

    fun prettyPrintExpr(expr: Expression, depth: Int): String {
        return when(expr){
            is Literal -> prettyPrintLiteral(expr.lit)
            is Var -> expr.ident.ident
            is Lambda -> "(\\${expr.binder.ident}. ${prettyPrintExpr(expr.body,0)})"
            is App -> {
                val output = "${prettyPrintExpr(expr.func, depth)} ${prettyPrintExpr(expr.arg, depth + 1)}"
                return if(depth > 0) "($output)" else output
            }
        }
    }

    fun prettyContext(ctx: Context): String {
        val context: String = ctx.toList().fold("", {acc, (ident, expr) ->
            if (ident.ident != "add") {
                acc + "${ident.ident} -> ${expr.pretty()}, "
            } else {
                acc
            }

        })
        return "[${context.dropLast(2)}]"
    }

    fun prettyPrintRTExpr(expr: RTExpression, depth: Int): String {
        return when(expr){
            is RTLiteral -> prettyPrintLiteral(expr.lit)
            is RTVar -> expr.ident.ident
            is RTLambda -> "(\\${expr.binder.ident}. ${prettyPrintRTExpr(expr.body,0)})"
            is RTClosure -> {
                "(\\${expr.binder.ident}. ${prettyPrintRTExpr(expr.body,0)})"
            }
            is RTApp -> {
                val output = "${prettyPrintRTExpr(expr.func, depth)} ${prettyPrintRTExpr(expr.arg, depth + 1)}"
                return if(depth > 0) "($output)" else output
            }
        }
    }

    fun prettyPrintType(type: Type, depth: Int): String {
        return when(type){
            Type.Int -> "Int"
            Type.Bool -> "Bool"
            is Type.Var -> type.ident.ident
            is Type.Fun ->
            {
                val output = "${prettyPrintType(type.arg, depth + 1)} -> ${prettyPrintType(type.result, 0)}"
                return if(depth > 0) "($output)" else output
            }
        }
    }

}
fun Expression.pretty() = Pretty.prettyPrintExpr(this, 0)
fun RTExpression.pretty() = Pretty.prettyPrintRTExpr(this, 0)
fun Context.pretty() = Pretty.prettyContext(this)
fun Type.pretty() = Pretty.prettyPrintType(this, 0)