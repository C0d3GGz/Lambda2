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
            is Type.ErrorSentinel -> type
            is Type.Var -> type
            is Type.Constructor -> Type.Constructor(type.name, type.tyArgs.map(::apply))
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
            is Expression.Let -> Expression.Let(
                expr.recursive,
                expr.binder,
                expr.scheme?.let { apply(it) },
                apply(expr.expr),
                apply(expr.body),
                expr.sp
            )
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

    override fun toString(): String {
        return "{ " + subst.toList().joinToString("\n, ") { (u, ty) -> "u$u ↦ ${ty.pretty()}" } + "\n}"
    }

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
            Name("concat") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.String,
                    Type.Fun(Type.String, Type.String)
                )
            ),
            Name("int_to_string") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.Int,
                    Type.String
                )
            ),
            Name("fix") to Scheme(
                listOf(TyVar(Name("a"))),
                Type.Fun(
                    Type.Fun(Type.v("a"), Type.v("a")),
                    Type.v("a")
                )
            ),
            Name("sleep") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.Int,
                    Type.Unit
                )
            ),
            Name("print") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.String,
                    Type.Unit
                )
            ),
            Name("clear") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.Unit,
                    Type.Unit
                )
            )
        )
    }

data class TypeInfo(val typeArgs: List<TyVar>, val dtors: List<DataConstructor>) {
    companion object {
        val empty = TypeInfo(emptyList(), emptyList())
    }
}

class Typechecker {

    private var fresh: Int = 0

    private val substitution: Substitution = Substitution.empty

    val types: MutableMap<Name, TypeInfo> = mutableMapOf(
        Name("Int") to TypeInfo.empty,
        Name("Bool") to TypeInfo.empty,
        Name("String") to TypeInfo.empty
    )

    fun freshUnknown(): Type.Unknown = Type.Unknown(++fresh)

    fun instantiate(scheme: Scheme): Type {
        val mappings = scheme.vars.map { it to freshUnknown() }
        return scheme.ty.substMany(mappings)
    }

    fun generalize(type: Type, ctx: TCContext): Scheme {
        val type = zonk(type)

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
            t1 is Type.Constructor &&
                    t2 is Type.Constructor &&
                    t1.name == t2.name &&
                    t1.tyArgs.size == t2.tyArgs.size -> try {
                t1.tyArgs.zip(t2.tyArgs).forEach { unify(it.first, it.second) }
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

    fun subsumes(lhs: Scheme, rhs: Scheme) {
        unify(instantiate(lhs), rhs.ty)
    }

    fun infer(ctx: TCContext, expr: Expression): Expression.Typed {
        val span = expr.span
        val tyWrap: (Expression, Type) -> Expression.Typed = { e, ty -> Expression.Typed(e, ty, span) }

        return when (expr) {
            is Expression.Literal -> {
                val t = when (expr.lit) {
                    is Lit.Int -> Type.Int
                    is Lit.Bool -> Type.Bool
                    is Lit.String -> Type.String
                }

                tyWrap(expr, t)
            }
            is Expression.Lambda -> {
                val tyBinder = freshUnknown()
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
                val tyRes = freshUnknown()
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
                val tyBinder = if (expr.recursive) {
                    val binderUnknown = freshUnknown()
                    val recCtx = HashMap(ctx)
                    recCtx[expr.binder] = Scheme.fromType(binderUnknown)

                    val binder = infer(recCtx, expr.expr)
                    unify(binderUnknown, binder.type)
                    binder
                } else {
                    infer(ctx, expr.expr)
                }

                val tmpCtx = HashMap(ctx)

                if (expr.scheme != null) {
                    subsumes(generalize(tyBinder.type, ctx), expr.scheme)
                    tmpCtx[expr.binder] = expr.scheme
                } else {
                    tmpCtx[expr.binder] = Scheme.fromType(tyBinder.type)
                }

                val tyBody = infer(tmpCtx, expr.body)

                tyWrap(Expression.Let(expr.recursive, expr.binder, expr.scheme, tyBinder, tyBody, span), tyBody.type)
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
                val (tyArgs, fields) = withSpannedError(Span(expr.type.span.start, expr.dtor.span.end)) {
                    lookupDtor(expr.type, expr.dtor)
                }

                val typedFields = mutableListOf<Expression.Typed>()
                val freshTyArgs = tyArgs.map { it to freshUnknown() }

                expr.exprs.zip(fields).forEach { (e, f) ->
                    val t = withSpannedError(e.span) { infer(ctx, e) }
                    withSpannedError(e.span) { unify(t.type, f.substMany(freshTyArgs)) }
                    typedFields += t
                }

                tyWrap(
                    Expression.Construction(expr.type, expr.dtor, typedFields, span),
                    Type.Constructor(expr.type, freshTyArgs.map { it.second })
                )
            }
            is Expression.Match -> {
                val tyRes = freshUnknown()
                val tyExpr = infer(ctx, expr.expr)
                val tyCases = mutableListOf<Expression.Case>()

                expr.cases.forEach { case ->
                    val tmpCtx = HashMap(ctx)

                    inferPattern(case.type, case.dtor, case.binders, tyExpr.type)
                        .forEach { (name, type) -> tmpCtx[name] = Scheme.fromType(type) }

                    val tyBody = infer(tmpCtx, case.body)
                    unify(tyBody.type, tyRes)

                    tyCases += case.copy(body = tyBody)
                }

                tyWrap(
                    Expression.Match(tyExpr, tyCases, span),
                    tyRes
                )
            }
        }
    }

    fun inferPattern(type: Name, dtor: Name, binders: List<Name>, tyExpr: Type): List<Pair<Name, Type>> {
        val (tyArgs, fields) = lookupDtor(type, dtor)
        val freshTyArgs = tyArgs.map { it to freshUnknown() }
        unify(Type.Constructor(type, freshTyArgs.map { it.second }), tyExpr)

        return binders.zip(fields.map { it.substMany(freshTyArgs) })
    }

    fun lookupDtor(type: Name, dtor: Name): Pair<List<TyVar>, List<Type>> {
        val typeInfo = types[type] ?: throw TypeError.UnknownType(type)
        val dc = typeInfo.dtors.find { it.name == dtor } ?: throw TypeError.UnknownDtor(type, dtor)
        return typeInfo.typeArgs to dc.fields
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
            types.putIfAbsent(it.name, TypeInfo(it.tyArgs, it.dataConstructors))
        }

        file.valueDeclarations().forEach {
            try {
                reset()
                val t = zonk(infer(ctx, it.expr))
                // println(this.substitution.toString())
                val scheme = generalize(t.type, ctx)

                withSpannedError(it.span) {
                    subsumes(scheme, it.scheme)
                }

                ctx[it.name] = it.scheme
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