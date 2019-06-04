package lambda

import java.lang.Exception
import io.vavr.collection.HashMap
import io.vavr.kotlin.*

typealias Context = HashMap<Ident, RTExpression>

sealed class RTExpression
data class RTLiteral(val lit: Lit) : RTExpression()
data class RTVar(val ident: Ident) : RTExpression()
data class RTLambda(val binder: Ident, val body: RTExpression) : RTExpression()
data class RTClosure(val binder: Ident, val body: RTExpression, val context: Context) : RTExpression()
data class RTApp(val func: RTExpression, val arg: RTExpression) : RTExpression()

fun fromExpr(expr: Expression): RTExpression {
    return when (expr) {
        is Literal -> RTLiteral(expr.lit)
        is Var -> RTVar(expr.ident)
        is Lambda -> RTLambda(expr.binder, fromExpr(expr.body))
        is App -> RTApp(fromExpr(expr.func), fromExpr(expr.arg))
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
    if (expr is RTLiteral && expr.lit is IntLit) {
        return expr.lit.int
    } else {
        throw EvalException("$expr is not an Int")
    }
}

fun eval(ctx: Context, expr: RTExpression): RTExpression {
    println("Evaling: ${ctx.pretty()} ${expr.pretty()}")
    return when (expr) {
        is RTLiteral -> expr
        is RTVar -> {
            when (expr.ident) {
                Ident("#add") -> {
                    RTLiteral(
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
        is RTLambda -> RTClosure(expr.binder, expr.body, ctx)
        is RTClosure -> expr
        is RTApp -> {
            when (val evaledClosure = eval(ctx, expr.func)) {
                is RTClosure -> {
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
        RTClosure(
            Ident("x"),
            RTLambda(
                Ident("y"),
                RTVar(Ident("#add"))
            ),
            hashMap()
        )
    val initialContext: Context = hashMap(Ident("add") to primAdd)
    return eval(initialContext, fromExpr(expr))
}

class EvalException(s: String) : Exception(s) {}
