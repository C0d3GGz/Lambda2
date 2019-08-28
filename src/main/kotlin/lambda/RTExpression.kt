package lambda

import io.vavr.collection.HashMap
import io.vavr.kotlin.hashMap
import lambda.syntax.*

typealias Context = HashMap<Name, RTExpression>

sealed class RTExpression {
    data class Literal(val lit: Lit) : RTExpression()
    data class Var(val name: Name) : RTExpression()
    data class Lambda(val binder: Name, val body: RTExpression) : RTExpression()
    data class Closure(val binder: Name, val body: RTExpression, val context: Context) : RTExpression()
    data class App(val func: RTExpression, val arg: RTExpression) : RTExpression()
    data class If(val condition: RTExpression, val thenBranch: RTExpression, val elseBranch: RTExpression) :
        RTExpression()
}

fun fromExpr(expr: Expression): RTExpression {
    return when (expr) {
        is Expression.Literal -> RTExpression.Literal(expr.lit)
        is Expression.Var -> RTExpression.Var(expr.name)
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
        is Expression.If -> RTExpression.If(
            fromExpr(expr.condition.value),
            fromExpr(expr.thenBranch.value),
            fromExpr(expr.elseBranch.value)
        )
        is Expression.Constructor -> TODO()
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
            when (expr.name) {
                Name("#add") -> {
                    RTExpression.Literal(
                        IntLit(
                            matchIntLiteral(ctx.get(Name("x")).get())
                                    + matchIntLiteral(ctx.get(Name("y")).get())
                        )
                    )
                }
                Name("#sub") -> {
                    RTExpression.Literal(
                        IntLit(
                            matchIntLiteral(ctx.get(Name("x")).get())
                                    - matchIntLiteral(ctx.get(Name("y")).get())
                        )
                    )
                }
                Name("#eq") -> {
                    RTExpression.Literal(
                        BoolLit(
                            matchIntLiteral(ctx.get(Name("x")).get())
                                    == matchIntLiteral(ctx.get(Name("y")).get())
                        )
                    )
                }
                else -> {
                    val res = ctx.get(expr.name)
                    return res.getOrElseThrow { EvalException("${expr.name} was undefined.") }
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
                else -> throw EvalException("$evaledClosure is not a function.")
            }
        }
        is RTExpression.If -> {
            val evalCondition = eval(ctx, expr.condition)

            if (evalCondition is RTExpression.Literal && evalCondition.lit is BoolLit) {
                if (evalCondition.lit.bool) eval(ctx, expr.thenBranch) else eval(ctx, expr.elseBranch)
            } else {
                throw EvalException("$evalCondition is not a bool.")
            }
        }
    }
}

fun evalExpr(expr: Expression): RTExpression {
    val primAdd: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.Var(Name("#add"))
            ),
            hashMap()
        )

    val primSub: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.Var(Name("#sub"))
            ),
            hashMap()
        )

    val primEq: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.Var(Name("#eq"))
            ),
            hashMap()
        )

    // Z = λf· (λx· f (λy· x x y)) (λx· f (λy· x x y))

    val innerZ = RTExpression.Lambda(
        Name("x"),
        RTExpression.App(
            RTExpression.Var(Name("f")),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.App(
                    RTExpression.App(
                        RTExpression.Var(Name("x")),
                        RTExpression.Var(Name("x"))
                    ),
                    RTExpression.Var(Name("y"))
                )
            )
        )
    )

    val z: RTExpression =
        RTExpression.Closure(
            Name("f"),
            RTExpression.App(
                innerZ,
                innerZ
            ),
            hashMap()
        )

    val initialContext: Context = hashMap(
        Name("add") to primAdd,
        Name("sub") to primSub,
        Name("eq") to primEq,
        Name("fix") to z
    )
    return eval(initialContext, fromExpr(expr))
}

class EvalException(s: String) : Exception(s) {}
