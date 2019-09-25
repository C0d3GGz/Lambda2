package lambda

import io.vavr.collection.HashMap
import io.vavr.control.Option
import io.vavr.kotlin.hashMap
import io.vavr.kotlin.hashSet
import lambda.syntax.*

typealias TCContext = HashMap<Name, Scheme>

data class Substitution(val subst: HashMap<Int, Type>) {
    fun get(u: Int): Option<Type> = subst.get(u)

    fun compose(that: Substitution): Substitution {
        return Substitution(that.subst.mapValues(::apply).merge(subst))
    }

    fun apply(type: Type): Type {
        return when (type) {
            is Type.Constructor, Type.ErrorSentinel -> type
            is Type.Var -> type
            is Type.Fun -> Type.Fun(apply(type.arg), apply(type.result))
            is Type.Unknown -> subst.getOrElse(type.u, type)
        }
    }

    fun apply(scheme: Scheme): Scheme {
        return Scheme(scheme.vars, apply(scheme.ty))
    }

    fun apply(ctx: TCContext): TCContext {
        return ctx.mapValues(::apply)
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

    fun <T> apply(type: Spanned<T>, f: (T) -> T): Spanned<T> {
        val (span, ty) = type
        return Spanned(span, f(ty))
    }

    fun apply(expr: Expression.Typed): Expression.Typed =
        Expression.Typed(apply(expr.expr), apply(expr.type), expr.sp)

    companion object {
        val empty = Substitution(hashMap())
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
        val list = Type.Constructor(Name("List"))

        return hashMap(
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
        val unknownsInCtx = ctx.values().map(Scheme::unknowns).fold(hashSet<Int>()) { a, b -> b.union(a) }
        val unknownVars = type.unknowns().removeAll(unknownsInCtx)
        val niceVars = ('a'..'z').iterator()

        var subst = hashMap<Int, Type>()
        val quantifier = mutableListOf<TyVar>()

        for (free in unknownVars) {
            val v = TyVar(Name(niceVars.nextChar().toString()))

            quantifier += v
            subst = subst.put(free, Type.Var(v))
        }

        return Scheme(quantifier, Substitution(subst).apply(type))
    }

    fun unify(t1: Type, t2: Type): Substitution {
        return when {
            t1 == t2 -> Substitution.empty
            t1 is Type.Unknown -> varBind(t1.u, t2)
            t2 is Type.Unknown -> varBind(t2.u, t1)
            t1 is Type.Fun && t2 is Type.Fun -> try {
                val s1 = unify(t1.arg, t2.arg)
                val s2 = unify(s1.apply(t1.result), s1.apply(t2.result))
                return s2.compose(s1)
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

    private fun varBind(u: Int, type: Type): Substitution {
        return if (type.unknowns().contains(u)) {
            throw TypeError.OccursCheck(u, type)
        } else {
            Substitution(hashMap(u to type))
        }
    }

    fun infer(ctx: TCContext, expr: Expression): Pair<Expression.Typed, Substitution> {
        val span = expr.span
        val tyWrap: (Expression, Type) -> Expression.Typed = { e, ty -> Expression.Typed(e, ty, span) }

        return when (expr) {
            is Expression.Literal -> {
                val t = when (expr.lit) {
                    is Lit.Int -> Type.Int
                    is Lit.Bool -> Type.Bool
                }
                tyWrap(expr, t) to Substitution.empty
            }
            is Expression.Lambda -> {
                val tyBinder = freshVar()
                val tmpCtx = ctx.put(expr.binder, Scheme(emptyList(), tyBinder))
                val (body, s) = withSpannedError(expr.body.span) {
                    this.infer(tmpCtx, expr.body)
                }
                s.apply(
                    tyWrap(
                        Expression.Lambda(expr.binder, body, span),
                        Type.Fun(tyBinder, body.type)
                    )
                ) to s
            }
            is Expression.Var -> {
                val scheme = ctx.get(expr.name)
                if (scheme.isDefined) {
                    // If we try to look up a value that failed to type check before we immediately bail
                    if (scheme.get().ty is Type.ErrorSentinel) throw TypeError.Followup()
                    val t = instantiate(scheme.get())
                    tyWrap(expr, t) to Substitution.empty
                } else {
                    throw TypeError.UnknownVar(expr.name, span)
                }
            }
            is Expression.App -> {
                val tyRes = freshVar()
                val (func, s1) = infer(ctx, expr.func)
                val (arg, s2) = infer(s1.apply(ctx), expr.arg)
                val s3 = withSpannedError(arg.span) { unify(s2.apply(func.type), Type.Fun(arg.type, tyRes)) }
                val s = s3.compose(s2).compose(s1)
                s.apply(tyWrap(Expression.App(func, arg, span), tyRes)) to s
            }
            is Expression.Typed -> {
                val (tyExpr, s) = infer(ctx, expr.expr)
                if (!expr.type.freeVars().isEmpty) { // TODO check wellformedness
                    throw RuntimeException("not allowed")
                }
                val s2 = withSpannedError(span) { unify(tyExpr.type, expr.type) }
                s2.apply(tyExpr) to s2.compose(s)
            }
            is Expression.Let -> {
                val (tyBinder, s1) = infer(ctx, expr.expr)
                val genBinder = generalize(tyBinder.type, s1.apply(ctx))

                val tmpCtx = s1.apply(ctx.put(expr.binder, genBinder))
                val (tyBody, s2) = infer(tmpCtx, s1.apply(expr.body))

                val s = s2.compose(s1)
                s.apply(tyWrap(Expression.Let(expr.binder, tyBinder, tyBody, span), tyBody.type)) to s
            }
            is Expression.If -> {
                val (tyCond, s1) = infer(ctx, expr.condition)
                val s2 = try {
                    unify(tyCond.type, Type.Bool)
                } catch (err: TypeError) {
                    throw TypeError.IfCondition(tyCond.type, expr.condition.span)
                }
                val ctx = s2.apply(s1.apply(ctx))

                val (tyThen, s3) = infer(ctx, expr.thenBranch)
                val (tyElse, s4) = infer(s3.apply(ctx), expr.elseBranch)

                val s5 = withSpannedError(span) { unify(s4.apply(tyThen.type), tyElse.type) }

                val s = s5.compose(s4).compose(s3).compose(s2).compose(s1)
                s.apply(tyWrap(Expression.If(tyCond, tyThen, tyElse, span), tyThen.type)) to s
            }

            is Expression.Construction -> {
                val fields = withSpannedError(Span(expr.type.span.start, expr.dtor.span.end)) {
                    lookupDtor(expr.type, expr.dtor)
                }

                val typedFields = mutableListOf<Expression.Typed>()
                var s = Substitution.empty

                expr.exprs.zip(fields).forEach { (e, f) ->
                    val (t, s1) = withSpannedError(e.span) { infer(ctx, s.apply(e)) }
                    val s2 = withSpannedError(e.span) { unify(t.type, f) }
                    typedFields.add(s2.apply(t))
                    s = s2.compose(s1).compose(s)
                }

                s.apply(
                    tyWrap(
                        Expression.Construction(expr.type, expr.dtor, typedFields, span),
                        Type.Constructor(expr.type)
                    )
                ) to s
            }
            is Expression.Match ->
                tyWrap(expr, Type.ErrorSentinel) to Substitution.empty
        }
    }

    fun lookupDtor(type: Name, dtor: Name): List<Type> {
        val dtors = types[type] ?: throw TypeError.UnknownType(type)
        val dc = dtors.find { it.name == dtor } ?: throw TypeError.UnknownDtor(type, dtor)
        return dc.fields
    }

    fun inferExpr(expr: Expression): Scheme {
        val (t, s) = try {
            this.infer(initialContext, expr)
        } catch (err: TypeError) {
            println("error: ${err.pretty()} ${if (err.span == Span.DUMMY) "" else err.span.toString()}")
            throw RuntimeException("type errors occurred")
        }
        println("inferred AST: ${t.pretty()}")
        return generalize(s.apply(t.type), initialContext)
    }

    fun inferSourceFile(file: SourceFile): HashMap<Name, Scheme> {
        var ctx = initialContext
        var errored = false

        file.typeDeclarations().forEach {
            types.putIfAbsent(it.name, it.dataConstructors)
        }

        file.valueDeclarations().forEach {
            try {
                val (t, s) = this.infer(ctx, it.expr)
                val scheme = generalize(s.apply(t.type), ctx)
                ctx = ctx.put(it.name, scheme)
            } catch (err: TypeError) {
                errored = true
                if (err !is TypeError.Followup) {
                    println("error ${if (err.span == Span.DUMMY) "" else err.span.toString()}: ${err.pretty()}")
                }
                ctx = ctx.put(it.name, Scheme.fromType(Type.ErrorSentinel))
            }
        }

        if (errored) throw Exception("Type errors occurred")

        return ctx.removeAll(initialContext.keysIterator())
    }
}