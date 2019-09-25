package lambda

import io.vavr.kotlin.hashSet
import lambda.syntax.*

fun <T, U> T?.fold(empty: U, f: (T) -> U) = this?.let(f) ?: empty

typealias TCContext = HashMap<Name, Scheme>

data class Substitution(val subst: HashMap<Int, Type>) {
    operator fun get(u: Int): Type? = subst[u]

    operator fun set(u: Int, type: Type) {
        subst[u] = type
    }

    fun apply(type: Type): Type {
        return when (type) {
            is Type.Constructor, Type.ErrorSentinel -> type
            is Type.Var -> type
            is Type.Fun -> Type.Fun(apply(type.arg), apply(type.result))
            is Type.Unknown -> subst[type.u].fold(type, ::apply)
        }
    }

    fun apply(scheme: Scheme): Scheme {
        return Scheme(scheme.vars, apply(scheme.ty))
    }

    fun apply(case: Expression.Case): Expression.Case {
        return case.copy(body = apply(case.body))
    }

    fun apply(expr: Expression): Expression {
        return when (expr) {
            is Expression.Literal, is Expression.Var -> expr
            is Expression.Lambda -> Expression.Lambda(expr.binder, apply(expr.body), expr.sp)
            is Expression.App -> Expression.App(apply(expr.func), apply(expr.arg), expr.sp)
            is Expression.Typed -> apply(expr)
            is Expression.Let -> Expression.Let(expr.binder, apply(expr.expr), apply(expr.body), expr.sp)
            is Expression.If -> Expression.If(
                apply(expr.condition),
                apply(expr.thenBranch),
                apply(expr.elseBranch),
                expr.sp
            )
            is Expression.Construction -> expr.copy(exprs = expr.exprs.map(::apply))
            is Expression.Match -> Expression.Match(apply(expr.expr), expr.cases.map(::apply), expr.sp)
        }
    }

    fun apply(expr: Expression.Typed): Expression.Typed =
        Expression.Typed(apply(expr.expr), apply(expr.type), expr.sp)

    companion object {
        val empty = Substitution(hashMapOf())
    }
}


sealed class TypeError : Exception() {
    open var span: Span? = null

    data class Unification(val ty1: Type, val ty2: Type, val stack: MutableList<Pair<Type, Type>>) : TypeError()
    data class UnknownVar(val name: Name, override var span: Span?) : TypeError()
    data class UnknownType(val name: Name) : TypeError()
    data class UnknownDtor(val type: Name, val name: Name) : TypeError()
    data class OccursCheck(val u: Int, val type: Type) : TypeError()
    data class IfCondition(val type: Type, override var span: Span?) : TypeError()
    class Followup() : TypeError()

    fun pretty(): String = when (this) {
        is Unification ->
            """Failed to match ${ty1.pretty()} with ${ty2.pretty()}
  ${stack.joinToString("\n  ") { (t1, t2) -> "while trying to match ${t1.pretty()} with ${t2.pretty()}" }}"""
        is UnknownVar -> "Unknown var $name"
        is UnknownType -> "Unknown type $name"
        is UnknownDtor -> "Type $type does not have a constructor named $name"
        is OccursCheck -> "Failed to infer the infinite type u$u ~ ${type.pretty()}"
        is IfCondition -> "Condition should be of type Bool but was ${type.pretty()}"
        is Followup -> "Followup error, you shouldn't be seeing this"
    }

    override fun toString(): String = this.pretty()
}

private val initialContext: TCContext
    get() {
        return hashMapOf(
            Name("add") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.Int,
                    Type.Fun(Type.Int, Type.Int)
                )
            ),
            Name("sub") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.Int,
                    Type.Fun(Type.Int, Type.Int)
                )
            ),
            Name("eq") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.Int,
                    Type.Fun(Type.Int, Type.Bool)
                )
            ),
            Name("identity") to Scheme(
                listOf(TyVar(Name("a"))),
                Type.Fun(Type.v("a"), Type.v("a"))
            ), // forall a. a -> a
            Name("fix") to Scheme(
                listOf(TyVar(Name("a"))),
                Type.Fun(
                    Type.Fun(Type.v("a"), Type.v("a")),
                    Type.v("a")
                )
            )
        )
    }

class Typechecker {

    private var fresh: Int = 0

    private val substitution: Substitution = Substitution.empty

    val types: MutableMap<Name, List<DataConstructor>> = mutableMapOf(
        Name("Int") to emptyList(),
        Name("Bool") to emptyList()
    )

    fun freshVar(): Type.Unknown = Type.Unknown(++fresh)

    fun instantiate(scheme: Scheme): Type {
        val mappings = scheme.vars.map { it to freshVar() }
        return scheme.ty.substMany(mappings)
    }

    fun generalize(type: Type, ctx: TCContext): Scheme {
        val unknownsInCtx = ctx.values.map(Scheme::unknowns).fold(hashSet<Int>()) { a, b -> b.union(a) }
        val unknownVars = type.unknowns().removeAll(unknownsInCtx)
        val niceVars = ('a'..'z').iterator()

        val subst = hashMapOf<Int, Type>()
        val quantifier = mutableListOf<TyVar>()

        for (free in unknownVars) {
            val v = TyVar(Name(niceVars.nextChar().toString()))

            quantifier += v
            subst[free] = Type.Var(v)
        }

        return Scheme(quantifier, Substitution(subst).apply(type))
    }

    fun unify(t1: Type, t2: Type) {
        val t1 = zonk(t1)
        val t2 = zonk(t2)

        when {
            t1 == t2 -> return
            t1 is Type.Unknown -> varBind(t1.u, t2)
            t2 is Type.Unknown -> varBind(t2.u, t1)
            t1 is Type.Fun && t2 is Type.Fun -> try {
                unify(t1.arg, t2.arg)
                unify(t1.result, t2.result)
            } catch (err: TypeError.Unification) {
                err.stack.add(t1 to t2)
                throw err
            }
            else -> throw TypeError.Unification(t1, t2, mutableListOf())
        }
    }

    fun <T> withSpannedError(span: Span, action: () -> T): T =
        try {
            action()
        } catch (err: TypeError) {
            err.span = err.span ?: span
            throw err
        }

    private fun varBind(u: Int, type: Type) {
        return if (type.unknowns().contains(u)) {
            throw TypeError.OccursCheck(u, type)
        } else {
            substitution[u] = type
        }
    }

    private fun zonk(type: Type): Type = substitution.apply(type)

    private fun zonk(type: Expression.Typed): Expression.Typed = substitution.apply(type)

    fun reset() {
        fresh = 0
        substitution.subst.clear()
    }

    fun infer(ctx: TCContext, expr: Expression): Expression.Typed {
        val span = expr.span
        val tyWrap: (Expression, Type) -> Expression.Typed = { e, ty -> Expression.Typed(e, ty, span) }

        return when (expr) {
            is Expression.Literal -> {
                val t = when (expr.lit) {
                    is Lit.Int -> Type.Int
                    is Lit.Bool -> Type.Bool
                }

                tyWrap(expr, t)
            }
            is Expression.Lambda -> {
                val tyBinder = freshVar()
                val tmpCtx = HashMap(ctx)
                tmpCtx[expr.binder] = Scheme.fromType(tyBinder)

                val body = withSpannedError(expr.body.span) {
                    this.infer(tmpCtx, expr.body)
                }

                tyWrap(
                    Expression.Lambda(expr.binder, body, span),
                    Type.Fun(tyBinder, body.type)
                )
            }
            is Expression.Var -> {
                val scheme = ctx[expr.name] ?: throw TypeError.UnknownVar(expr.name, span)

                // If we try to look up a value that failed to type check before we immediately bail out
                if (scheme.ty is Type.ErrorSentinel) throw TypeError.Followup()
                tyWrap(expr, instantiate(scheme))
            }
            is Expression.App -> {
                val tyRes = freshVar()
                val func = infer(ctx, expr.func)
                val arg = infer(ctx, expr.arg)

                withSpannedError(arg.span) { unify(func.type, Type.Fun(arg.type, tyRes)) }
                tyWrap(Expression.App(func, arg, span), tyRes)
            }
            is Expression.Typed -> {
                val tyExpr = infer(ctx, expr.expr)
                if (!expr.type.freeVars().isEmpty) { // TODO check wellformedness
                    throw RuntimeException("not allowed")
                }

                withSpannedError(span) { unify(tyExpr.type, expr.type) }
                tyExpr
            }
            is Expression.Let -> {
                val tyBinder = infer(ctx, expr.expr)
                val genBinder = generalize(tyBinder.type, ctx)

                val tmpCtx = HashMap(ctx)
                tmpCtx[expr.binder] = genBinder

                val tyBody = infer(tmpCtx, expr.body)

                tyWrap(Expression.Let(expr.binder, tyBinder, tyBody, span), tyBody.type)
            }
            is Expression.If -> {
                val tyCond = infer(ctx, expr.condition)

                try {
                    unify(tyCond.type, Type.Bool)
                } catch (err: TypeError) {
                    throw TypeError.IfCondition(tyCond.type, expr.condition.span)
                }

                val tyThen = infer(ctx, expr.thenBranch)
                val tyElse = infer(ctx, expr.elseBranch)

                withSpannedError(span) { unify(tyThen.type, tyElse.type) }

                tyWrap(Expression.If(tyCond, tyThen, tyElse, span), tyThen.type)
            }

            is Expression.Construction -> {
                val fields = withSpannedError(Span(expr.type.span.start, expr.dtor.span.end)) {
                    lookupDtor(expr.type, expr.dtor)
                }

                val typedFields = mutableListOf<Expression.Typed>()

                expr.exprs.zip(fields).forEach { (e, f) ->
                    val t = withSpannedError(e.span) { infer(ctx, e) }
                    withSpannedError(e.span) { unify(t.type, f) }
                    typedFields += t
                }

                tyWrap(
                    Expression.Construction(expr.type, expr.dtor, typedFields, span),
                    Type.Constructor(expr.type)
                )
            }
            is Expression.Match ->
                tyWrap(expr, Type.ErrorSentinel)
        }
    }

    fun lookupDtor(type: Name, dtor: Name): List<Type> {
        val dtors = types[type] ?: throw TypeError.UnknownType(type)
        val dc = dtors.find { it.name == dtor } ?: throw TypeError.UnknownDtor(type, dtor)
        return dc.fields
    }

    fun inferExpr(expr: Expression): Scheme {
        val t = try {
            reset()
            zonk(infer(initialContext, expr))
        } catch (err: TypeError) {
            println("error: ${err.pretty()} ${if (err.span == Span.DUMMY) "" else err.span.toString()}")
            throw RuntimeException("type errors occurred")
        }
        println("inferred AST: ${t.pretty()}")
        return generalize(t.type, initialContext)
    }

    fun inferSourceFile(file: SourceFile): HashMap<Name, Scheme> {
        val ctx = initialContext
        var errored = false

        file.typeDeclarations().forEach {
            types.putIfAbsent(it.name, it.dataConstructors)
        }

        file.valueDeclarations().forEach {
            try {
                reset()
                val t = zonk(infer(ctx, it.expr))
                val scheme = generalize(t.type, ctx)
                ctx[it.name] = scheme
            } catch (err: TypeError) {
                errored = true
                if (err !is TypeError.Followup) {
                    println("error ${if (err.span == Span.DUMMY) "" else err.span.toString()}: ${err.pretty()}")
                }
                ctx[it.name] = Scheme.fromType(Type.ErrorSentinel)
            }
        }

        if (errored) throw Exception("Type errors occurred")

        initialContext.forEach { (t, _) -> ctx.remove(t) }
        return ctx
    }
}