package lambda

import io.vavr.collection.HashMap
import io.vavr.kotlin.hashMap

typealias Context = HashMap<Ident, RTExpression>

sealed class RTExpression {
    data class Literal(val lit: Lit) : RTExpression()
    data class Var(val ident: Ident) : RTExpression()
    data class Lambda(val binder: Ident, val body: RTExpression) : RTExpression()
    data class Closure(val binder: Ident, val body: RTExpression, val context: Context) : RTExpression()
    data class App(val func: RTExpression, val arg: RTExpression) : RTExpression()
}

fun fromExpr(expr: Expression): RTExpression {
    return when (expr) {
        is Expression.Literal -> RTExpression.Literal(expr.lit)
        is Expression.Var -> RTExpression.Var(expr.ident)
        is Expression.Lambda -> RTExpression.Lambda(expr.binder.value, fromExpr(expr.body.value))
        is Expression.App -> RTExpression.App(fromExpr(expr.func.value), fromExpr(expr.arg.value))
        is Expression.Typed -> fromExpr(expr.expr.value)
        is Expression.Let -> {
            // let x = 4 in add x 5
            // (\x. add x 5) 4

            RTExpression.App(
                RTExpression.Lambda(expr.binder.value, fromExpr(expr.body.value)),
                fromExpr(expr.expr.value)
            )
        }
    }
}

// (\x. x 1 2) : ?
// (\f. \g. f g) : ?
// (\x. x) (\y. y) 1 : ?
// (\x. x) 1 : ?
// (\f. add 1 (f 3)) : ?
// (\f. f (add 1) 1) : ?
// (\f. \g. g (f 1) 1) : ?

fun matchIntLiteral(expr: RTExpression): Int {
    if (expr is RTExpression.Literal && expr.lit is IntLit) {
        return expr.lit.int
    } else {
        throw EvalException("$expr is not an Int")
    }
}

fun eval(ctx: Context, expr: RTExpression): RTExpression {
    println("Evaling: ${ctx.pretty()} ${expr.pretty()}")
    return when (expr) {
        is RTExpression.Literal -> expr
        is RTExpression.Var -> {
            when (expr.ident) {
                Ident("#add") -> {
                    RTExpression.Literal(
                        IntLit(
                            matchIntLiteral(ctx.get(Ident("x")).get())
                                    + matchIntLiteral(ctx.get(Ident("y")).get())
                        )
                    )
                }
                else -> {
                    val res = ctx.get(expr.ident)
                    return res.getOrElseThrow { EvalException("${expr.ident} was undefined.") }
                }
            }
        }
        is RTExpression.Lambda -> RTExpression.Closure(expr.binder, expr.body, ctx)
        is RTExpression.Closure -> expr
        is RTExpression.App -> {
            when (val evaledClosure = eval(ctx, expr.func)) {
                is RTExpression.Closure -> {
                    val evaledArg = eval(ctx, expr.arg)
                    val tmpCtx = evaledClosure.context.put(evaledClosure.binder, evaledArg)
                    eval(tmpCtx, evaledClosure.body)
                }
                else -> throw EvalException("${evaledClosure} is not a function.")
            }
        }
    }
}

fun evalExpr(expr: Expression): RTExpression {
    val primAdd: RTExpression =
        RTExpression.Closure(
            Ident("x"),
            RTExpression.Lambda(
                Ident("y"),
                RTExpression.Var(Ident("#add"))
            ),
            hashMap()
        )
    val initialContext: Context = hashMap(Ident("add") to primAdd)
    return eval(initialContext, fromExpr(expr))
}

class EvalException(s: String) : Exception(s) {}
