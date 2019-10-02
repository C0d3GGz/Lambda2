package lambda

import lambda.syntax.Lit
import lambda.syntax.Name

typealias Context = HashMap<Name, RTExpression>

sealed class RTExpression {
    data class Literal(val lit: Lit) : RTExpression()
    data class Var(val name: Name) : RTExpression()
    data class Lambda(val binder: Name, val body: RTExpression) : RTExpression()
    data class Closure(val binder: Name, val body: RTExpression, val context: Context) : RTExpression()
    data class App(val func: RTExpression, val arg: RTExpression) : RTExpression()
    data class If(val condition: RTExpression, val thenBranch: RTExpression, val elseBranch: RTExpression) :
        RTExpression()

    data class LetRec(val binder: Name, val expr: RTExpression, val body: RTExpression) : RTExpression()

    data class Pack(val tag: Int, val data: List<RTExpression>) : RTExpression() {
        val arity: Int
            get() = data.size
    }

    data class Match(val expr: RTExpression, val cases: List<Case>) : RTExpression()

    data class Case(val tag: Int, val binders: List<Name>, val body: RTExpression)
}

// (\x. x 1 2) : ?
// (\f. \g. f g) : ?
// (\x. x) (\y. y) 1 : ?
// (\x. x) 1 : ?
// (\f. add 1 (f 3)) : ?
// (\f. f (add 1) 1) : ?
// (\f. \g. g (f 1) 1) : ?

fun matchIntLiteral(expr: RTExpression): Int {
    if (expr is RTExpression.Literal && expr.lit is Lit.Int) {
        return expr.lit.int
    } else {
        throw EvalException("$expr is not an Int")
    }
}

fun eval(ctx: Context, expr: RTExpression): RTExpression {
    // println("Evaling: ${ctx.pretty()} ${expr.pretty()}")
    return when (expr) {
        is RTExpression.Literal -> expr
        is RTExpression.Var -> {
            when (expr.name) {
                Name("#add") -> {
                    RTExpression.Literal(
                        Lit.Int(
                            matchIntLiteral(ctx[Name("x")]!!)
                                    + matchIntLiteral(ctx[Name("y")]!!)
                        )
                    )
                }
                Name("#sub") -> {
                    RTExpression.Literal(
                        Lit.Int(
                            matchIntLiteral(ctx[Name("x")]!!)
                                    - matchIntLiteral(ctx[Name("y")]!!)
                        )
                    )
                }
                Name("#eq") -> {
                    RTExpression.Literal(
                        Lit.Bool(
                            matchIntLiteral(ctx[Name("x")]!!)
                                    == matchIntLiteral(ctx[Name("y")]!!)
                        )
                    )
                }
                else -> ctx[expr.name] ?: throw EvalException("${expr.name} was undefined.")
            }
        }
        is RTExpression.Lambda -> RTExpression.Closure(expr.binder, expr.body, ctx)
        is RTExpression.Closure -> expr
        is RTExpression.App -> {
            when (val evaledClosure = eval(ctx, expr.func)) {
                is RTExpression.Closure -> {
                    val evaledArg = eval(ctx, expr.arg)
                    val tmpCtx = HashMap(evaledClosure.context)
                    tmpCtx[evaledClosure.binder] = evaledArg
                    eval(tmpCtx, evaledClosure.body)
                }
                else -> throw EvalException("$evaledClosure is not a function.")
            }
        }
        is RTExpression.If -> {
            val evalCondition = eval(ctx, expr.condition)

            if (evalCondition is RTExpression.Literal && evalCondition.lit is Lit.Bool) {
                if (evalCondition.lit.bool) eval(ctx, expr.thenBranch) else eval(ctx, expr.elseBranch)
            } else {
                throw EvalException("$evalCondition is not a bool.")
            }
        }
        is RTExpression.Pack -> RTExpression.Pack(
            expr.tag,
            expr.data.map { eval(ctx, it) }
        )
        is RTExpression.Match -> {
            val evaledExpr = eval(ctx, expr.expr) as? RTExpression.Pack
                ?: throw EvalException("tried to pattern match on a non-pack value")
            val case = expr.cases.firstOrNull { it.tag == evaledExpr.tag }
                ?: throw EvalException("failed to match pattern with tag ${evaledExpr.tag}")
            val tmpCtx = HashMap(ctx)

            case.binders.zip(evaledExpr.data).forEach { (n, e) ->
                tmpCtx[n] = e
            }

            eval(tmpCtx, case.body)
        }
        is RTExpression.LetRec -> {
            val evaledExpr = eval(ctx, expr.expr)

            if (evaledExpr is RTExpression.Closure) {
                evaledExpr.context[expr.binder] = evaledExpr
            }

            val bodyCtx = HashMap(ctx)
            bodyCtx[expr.binder] = evaledExpr

            eval(bodyCtx, expr.body)
        }
    }
}

fun evalExprs(exprs: List<Pair<Name, RTExpression>>): RTExpression {
    val ctx = initialContext()
    var result: RTExpression = RTExpression.Var(Name("empty source file"))

    exprs.forEach {
        result = eval(ctx, it.second)
        ctx[it.first] = result
    }

    return result
}

fun evalExpr(expr: RTExpression): RTExpression {
    return eval(initialContext(), expr)
}


private fun initialContext(): Context {
    val primAdd: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.Var(Name("#add"))
            ),
            hashMapOf()
        )

    val primSub: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.Var(Name("#sub"))
            ),
            hashMapOf()
        )

    val primEq: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.Var(Name("#eq"))
            ),
            hashMapOf()
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
            hashMapOf()
        )

    return hashMapOf(
        Name("add") to primAdd,
        Name("sub") to primSub,
        Name("eq") to primEq,
        Name("fix") to z
    )
}

class EvalException(s: String) : Exception(s)
