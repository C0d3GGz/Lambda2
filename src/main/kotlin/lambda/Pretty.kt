package lambda

import lambda.syntax.*

private object Pretty {

    fun prettyPrintLiteral(lit: Lit): String {
        return when (lit) {
            is Lit.Int -> lit.int.toString()
            is Lit.Bool -> lit.bool.toString()
            is Lit.String -> "\"${lit.string}\""
        }
    }

    fun prettyPrintExpr(expr: Expression, depth: Int): String {
        return when (expr) {
            is Expression.Literal -> prettyPrintLiteral(expr.lit)
            is Expression.Var -> "${expr.name}"
            is Expression.Lambda -> "(\\${expr.binder}. ${prettyPrintExpr(expr.body, 0)})"
            is Expression.App -> {
                val output = "${prettyPrintExpr(expr.func, depth)} ${prettyPrintExpr(expr.arg, depth + 1)}"
                return if (depth > 0) "($output)" else output
            }
            is Expression.Typed -> "(${expr.expr.pretty()} : ${expr.type.pretty()})"
            is Expression.Let ->
                "let ${expr.binder} = ${expr.expr.pretty()} in ${expr.body.pretty()}"
            is Expression.If ->
                "if ${expr.condition.pretty()} then ${expr.thenBranch.pretty()} else ${expr.elseBranch.pretty()}"
            is Expression.Construction ->
                "${expr.type}::${expr.dtor}(${expr.exprs.joinToString(", ") { it.pretty() }})"
            is Expression.Match ->
                "match ${expr.expr.pretty()} {${expr.cases.joinToString(", ") { it.pretty() }}}"
        }
    }

    fun prettyContext(ctx: Context): String {
        val context: String = ctx.toList().fold("") { acc, (name, expr) ->
            if (name.value != "add") {
                acc + "${name.value} -> ${expr.pretty()}, "
            } else {
                acc
            }
        }

        return "[${context.dropLast(2)}]"
    }

    fun prettyPrintRTExpr(expr: RTExpression, depth: Int): String {
        return when (expr) {
            is RTExpression.Literal -> prettyPrintLiteral(expr.lit)
            is RTExpression.Var -> "${expr.name}"
            is RTExpression.Lambda -> "(\\${expr.binder}. ${prettyPrintRTExpr(expr.body, 0)})"
            is RTExpression.Closure -> {
                "(\\${expr.binder.value}. ${prettyPrintRTExpr(expr.body, 0)})"
            }
            is RTExpression.App -> {
                val output = "${prettyPrintRTExpr(expr.func, depth)} ${prettyPrintRTExpr(expr.arg, depth + 1)}"
                return if (depth > 0) "($output)" else output
            }
            is RTExpression.If ->
                "if ${expr.condition.pretty()} then ${expr.thenBranch.pretty()} else ${expr.elseBranch.pretty()}"
            is RTExpression.Pack ->
                "Pack { tag: ${expr.tag}, arity: ${expr.arity}, data: [${expr.data.joinToString(", ") { it.pretty() }}]}"
            is RTExpression.Match ->
                "match ${expr.expr.pretty()} {${expr.cases.joinToString(", ") { it.pretty() }}}"
            is RTExpression.LetRec ->
                "letrec ${expr.binder} = ${expr.expr.pretty()} in ${expr.body.pretty()}"
        }
    }

    fun prettyPrintType(type: Type, depth: Int): String {
        return when (type) {
            is Type.Constructor -> "${type.name}${if (type.tyArgs.isEmpty()) "" else "<${type.tyArgs.joinToString(", ") { it.pretty() }}>"}"
            is Type.Var -> "${type.v.name}"
            is Type.Fun -> {
                val output = "${prettyPrintType(type.arg, depth + 1)} -> ${prettyPrintType(type.result, 0)}"
                return if (depth > 0) "($output)" else output
            }
            Type.ErrorSentinel -> "ERR"
            is Type.Unknown -> "$type"
        }
    }

    fun prettyPrintScheme(scheme: Scheme): String {
        return if (scheme.vars.isEmpty()) scheme.ty.pretty()
        else "forall ${scheme.vars.joinToString(" ")}. ${scheme.ty.pretty()}"
    }


    fun prettyPrintCase(case: Expression.Case): String {
        return "${case.type}::${case.dtor}(${case.binders.joinToString(", ")}) => ${case.body.pretty()}"
    }

    fun prettyPrintRTCase(case: RTExpression.Case): String {
        return "<${case.tag}>(${case.binders.joinToString(", ")}) => ${case.body.pretty()}"
    }
}

fun Expression.pretty() = Pretty.prettyPrintExpr(this, 0)
fun RTExpression.pretty() = Pretty.prettyPrintRTExpr(this, 0)
fun Context.pretty() = Pretty.prettyContext(this)
fun Type.pretty() = Pretty.prettyPrintType(this, 0)
fun Scheme.pretty() = Pretty.prettyPrintScheme(this)
fun Expression.Case.pretty() = Pretty.prettyPrintCase(this)
fun RTExpression.Case.pretty() = Pretty.prettyPrintRTCase(this)