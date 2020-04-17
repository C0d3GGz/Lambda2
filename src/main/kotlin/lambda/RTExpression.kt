package lambda

import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.persistentHashMapOf
import lambda.syntax.Lit
import lambda.syntax.Name
import lambda.syntax.Namespace

inline class Context(val binders: PersistentMap<Namespaced, RTExpression>) {
    operator fun get(name: Namespaced): RTExpression? = binders[name]

    fun getLocal(name: String): RTExpression = get(Namespace.local to Name(name))!!
    fun extend(binders: PersistentMap<Namespaced, RTExpression>): Context = Context(this.binders.putAll(binders))
    fun extendLocal(name: Name, expr: RTExpression): Context = extend(persistentHashMapOf(Namespace.local to name to expr))
    fun toList(): List<Pair<Namespaced, RTExpression>> = binders.toList()

    companion object {
        fun empty() = Context(persistentHashMapOf())
    }
}

sealed class RTExpression {
    data class Literal(val lit: Lit) : RTExpression()
    data class Var(val namespace: Namespace, val name: Name) : RTExpression()
    data class Lambda(val binder: Name, val body: RTExpression) : RTExpression()
    data class Closure(val binder: Name, val body: RTExpression, var context: Context) : RTExpression()
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

fun matchIntLiteral(expr: RTExpression): Int {
    if (expr is RTExpression.Literal && expr.lit is Lit.Int) {
        return expr.lit.int
    } else {
        throw EvalException("$expr is not an Int")
    }
}

fun matchStringLiteral(expr: RTExpression): String {
    if (expr is RTExpression.Literal && expr.lit is Lit.String) {
        return expr.lit.string
    } else {
        throw EvalException("$expr is not a String")
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
                            matchIntLiteral(ctx.getLocal("x"))
                                    + matchIntLiteral(ctx.getLocal("y"))
                        )
                    )
                }
                Name("#sub") -> {
                    RTExpression.Literal(
                        Lit.Int(
                            matchIntLiteral(ctx.getLocal("x"))
                                    - matchIntLiteral(ctx.getLocal("y"))
                        )
                    )
                }
                Name("#eq") -> {
                    RTExpression.Literal(
                        Lit.Bool(
                            matchIntLiteral(ctx.getLocal("x"))
                                    == matchIntLiteral(ctx.getLocal("y"))
                        )
                    )
                }
                Name("#concat") -> {
                    RTExpression.Literal(
                        Lit.String(
                            matchStringLiteral(ctx.getLocal("x"))
                                    + matchStringLiteral(ctx.getLocal("y"))
                        )
                    )
                }
                Name("#int_to_string") -> {
                    RTExpression.Literal(
                        Lit.String(
                            matchIntLiteral(ctx.getLocal("x")).toString()
                        )
                    )
                }
                Name("#sleep") -> {
                    val x = matchIntLiteral(ctx.getLocal("x"))
                    Thread.sleep(x.toLong())
                    RTExpression.Pack(1, emptyList())
                }
                Name("#print") -> {
                    val x = matchStringLiteral(ctx.getLocal("x"))
                    println(x)
                    RTExpression.Pack(1, emptyList())
                }
                Name("#clear") -> {
                    print("\u001b[H\u001b[2J")
                    RTExpression.Pack(1, emptyList())
                }
                else -> ctx[expr.namespace to expr.name]
                    ?: throw EvalException("${expr.namespace.asQualifier()}${expr.name} was undefined.")
            }
        }
        is RTExpression.Lambda -> RTExpression.Closure(expr.binder, expr.body, ctx)
        is RTExpression.Closure -> expr
        is RTExpression.App -> {
            when (val evaledClosure = eval(ctx, expr.func)) {
                is RTExpression.Closure -> {
                    val evaledArg = eval(ctx, expr.arg)
                    val tmpCtx = evaledClosure.context.extendLocal(evaledClosure.binder, evaledArg)
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
            val binders = case.binders
                .zip(evaledExpr.data)
                .fold(persistentHashMapOf<Namespaced, RTExpression>()) { acc, (n, e) ->
                    acc.put(Namespace.local to n, e)
                }

            eval(ctx.extend(binders), case.body)
        }
        is RTExpression.LetRec -> {
            val evaledExpr = eval(ctx, expr.expr)

            if (evaledExpr is RTExpression.Closure) {
                evaledExpr.context = evaledExpr.context.extendLocal(expr.binder, evaledExpr)
            }

            eval(ctx.extendLocal(expr.binder, evaledExpr), expr.body)
        }
    }
}

fun evalExprs(ctx: Context, namespace: Namespace, exprs: List<Pair<Name, RTExpression>>): Pair<Context, RTExpression> {
    var result: RTExpression = RTExpression.Var(namespace, Name("empty source file"))

    val resCtx = exprs.fold(ctx) { acc, (name, expr) ->
        result = eval(acc, expr)
        acc.extend(persistentHashMapOf(namespace to name to result))
    }

    return resCtx to result
}

fun evalExpr(expr: RTExpression): RTExpression {
    return eval(initialContext(), expr)
}

fun initialContext(): Context {
    val primAdd: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.Var(Namespace.local, Name("#add"))
            ),
            Context.empty()
        )

    val primSub: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.Var(Namespace.local, Name("#sub"))
            ),
            Context.empty()
        )

    val primEq: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.Var(Namespace.local, Name("#eq"))
            ),
            Context.empty()
        )
    val primConcat: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.Var(Namespace.local, Name("#concat"))
            ),
            Context.empty()
        )

    val primint_to_string: RTExpression =
        RTExpression.Closure(
            Name("x"),
            RTExpression.Var(Namespace.local, Name("#int_to_string")),
            Context.empty()
        )

    // Z = λf· (λx· f (λy· x x y)) (λx· f (λy· x x y))

    val innerZ = RTExpression.Lambda(
        Name("x"),
        RTExpression.App(
            RTExpression.Var(Namespace.local, Name("f")),
            RTExpression.Lambda(
                Name("y"),
                RTExpression.App(
                    RTExpression.App(
                        RTExpression.Var(Namespace.local, Name("x")),
                        RTExpression.Var(Namespace.local, Name("x"))
                    ),
                    RTExpression.Var(Namespace.local, Name("y"))
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
            Context.empty()
        )

    val primSleep = RTExpression.Closure(
        Name("x"),
        RTExpression.Var(Namespace.local, Name("#sleep")),
        Context.empty()
    )

    val primPrint = RTExpression.Closure(
        Name("x"),
        RTExpression.Var(Namespace.local, Name("#print")),
        Context.empty()
    )

    val primClear = RTExpression.Closure(
        Name("x"),
        RTExpression.Var(Namespace.local, Name("#clear")),
        Context.empty()
    )

    val primUnit = RTExpression.Pack(1, listOf())

    return Context(
        persistentHashMapOf(
            Namespace.prim to Name("add") to primAdd,
            Namespace.prim to Name("sub") to primSub,
            Namespace.prim to Name("eq") to primEq,
            Namespace.prim to Name("concat") to primConcat,
            Namespace.prim to Name("int_to_string") to primint_to_string,
            Namespace.prim to Name("fix") to z,
            Namespace.prim to Name("sleep") to primSleep,
            Namespace.prim to Name("print") to primPrint,
            Namespace.prim to Name("clear") to primClear,
            Namespace.prim to Name("unit") to primUnit
        )
    )
}

class EvalException(s: String) : Exception(s)
