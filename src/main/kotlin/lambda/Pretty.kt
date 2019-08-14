package lambda

import io.vavr.kotlin.component1
import io.vavr.kotlin.component2

private object Pretty {

    fun prettyPrintLiteral(lit: Lit): String {
        return when (lit) {
            is IntLit -> lit.int.toString()
            is BoolLit -> lit.bool.toString()
        }
    }

    fun prettyPrintExpr(expr: Expression, depth: Int): String {
        return when (expr) {
            is Expression.Literal -> prettyPrintLiteral(expr.lit)
            is Expression.Var -> expr.ident.ident
            is Expression.Lambda -> "(\\${expr.binder.value.ident}. ${prettyPrintExpr(expr.body.value, 0)})"
            is Expression.App -> {
                val output = "${prettyPrintExpr(expr.func.value, depth)} ${prettyPrintExpr(expr.arg.value, depth + 1)}"
                return if (depth > 0) "($output)" else output
            }
            is Expression.Typed -> "(${expr.expr.value.pretty()} : ${expr.type.value.pretty()})"
            is Expression.Let -> "let ${expr.binder.value.ident} = ${expr.expr.value.pretty()} in ${expr.body.value.pretty()}"
            is Expression.If -> "if ${expr.condition.value.pretty()} then ${expr.thenBranch.value.pretty()} else ${expr.elseBranch.value.pretty()}"
        }
    }

    fun prettyContext(ctx: Context): String {
        val context: String = ctx.toList().fold("", { acc, (ident, expr) ->
            if (ident.ident != "add") {
                acc + "${ident.ident} -> ${expr.pretty()}, "
            } else {
                acc
            }

        })
        return "[${context.dropLast(2)}]"
    }

    fun prettyPrintRTExpr(expr: RTExpression, depth: Int): String {
        return when (expr) {
            is RTExpression.Literal -> prettyPrintLiteral(expr.lit)
            is RTExpression.Var -> expr.ident.ident
            is RTExpression.Lambda -> "(\\${expr.binder.ident}. ${prettyPrintRTExpr(expr.body, 0)})"
            is RTExpression.Closure -> {
                "(\\${expr.binder.ident}. ${prettyPrintRTExpr(expr.body, 0)})"
            }
            is RTExpression.App -> {
                val output = "${prettyPrintRTExpr(expr.func, depth)} ${prettyPrintRTExpr(expr.arg, depth + 1)}"
                return if (depth > 0) "($output)" else output
            }
            is RTExpression.If -> "if ${expr.condition.pretty()} then ${expr.thenBranch.pretty()} else ${expr.elseBranch.pretty()}"
        }
    }

    fun prettyPrintType(type: Type, depth: Int): String {
        return when (type) {
            Type.Int -> "Int"
            Type.Bool -> "Bool"
            is Type.Var -> type.ident.ident
            is Type.Fun -> {
                val output = "${prettyPrintType(type.arg.value, depth + 1)} -> ${prettyPrintType(type.result.value, 0)}"
                return if (depth > 0) "($output)" else output
            }
            Type.ErrorSentinel -> "ERR"
        }
    }

    fun prettyPrintScheme(scheme: Scheme): String {
        return if (scheme.vars.isEmpty()) scheme.ty.pretty()
        else "forall ${scheme.vars.map(Ident::ident).joinToString(" ")}. ${scheme.ty.pretty()}"
    }

    fun prettyPrintEvalExpr(expr: EvalExpression, depth: Int): String {
        return when (expr) {
            is EvalExpression.Literal -> prettyPrintLiteral(expr.lit)
            is EvalExpression.Var -> expr.ident.ident
            is EvalExpression.Lambda -> "(\\${expr.binder.ident}. ${expr.body.pretty()})"
            is EvalExpression.App -> {
               val output = "${prettyPrintEvalExpr(expr.func, depth)} ${prettyPrintEvalExpr(expr.arg, depth + 1)}"
               return if (depth > 0) "($output)" else output
            }
            is EvalExpression.Typed -> "(${expr.expr.pretty()} : ${expr.type.pretty()})"
            is EvalExpression.Let -> "let ${expr.binder.ident} = ${expr.expr.pretty()} in ${expr.body.pretty()}"
            is EvalExpression.If -> "if ${expr.condition.pretty()} then ${expr.thenBranch.pretty()} else ${expr.elseBranch.pretty()}"
        }
    }

}

fun Expression.pretty() = Pretty.prettyPrintExpr(this, 0)
fun EvalExpression.pretty() = Pretty.prettyPrintEvalExpr(this, 0)
fun RTExpression.pretty() = Pretty.prettyPrintRTExpr(this, 0)
fun Context.pretty() = Pretty.prettyContext(this)
fun Type.pretty() = Pretty.prettyPrintType(this, 0)
fun Scheme.pretty() = Pretty.prettyPrintScheme(this)