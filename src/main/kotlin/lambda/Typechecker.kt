package lambda

import io.vavr.collection.HashMap
import io.vavr.control.Either
import io.vavr.control.Option
import io.vavr.kotlin.hashMap
import io.vavr.kotlin.hashSet

typealias TCContext = HashMap<Ident, Scheme>

data class Substitution(val subst: HashMap<Ident, Type>) {
    fun get(ident: Ident): Option<Type> = subst.get(ident)

    fun compose(that: Substitution): Substitution {
        return Substitution(that.subst.mapValues(::apply).merge(subst))
    }

    fun apply(type: Type): Type {
        return when (type) {
            Type.Int, Type.Bool, Type.ErrorSentinel -> type
            is Type.Var -> get(type.ident).getOrElse(type)
            is Type.Fun -> Type.Fun(apply(type.arg, ::apply), apply(type.result, ::apply))
        }
    }

    fun apply(scheme: Scheme): Scheme {
        val tmpS = Substitution(this.subst.removeAll(scheme.vars))
        return Scheme(scheme.vars, tmpS.apply(scheme.ty))
    }

    fun apply(ctx: TCContext): TCContext {
        return ctx.mapValues(::apply)
    }

    fun apply(expr: Expression): Expression {
        return when (expr) {
            is Expression.Literal, is Expression.Var -> expr
            is Expression.Lambda -> Expression.Lambda(expr.binder, apply(expr.body, ::apply))
            is Expression.App -> Expression.App(apply(expr.func, ::apply), apply(expr.arg, ::apply))
            is Expression.Typed -> apply(expr)
            is Expression.Let -> Expression.Let(expr.binder, apply(expr.expr, ::apply), apply(expr.body, ::apply))
            is Expression.If -> Expression.If(
                apply(expr.condition, ::apply),
                apply(expr.thenBranch, ::apply),
                apply(expr.elseBranch, ::apply)
            )
        }
    }

    fun <T> apply(type: Spanned<T>, f: (T) -> T): Spanned<T> {
        val (span, ty) = type
        return Spanned(span, f(ty))
    }

    fun apply(expr: Expression.Typed): Expression.Typed =
        Expression.Typed(apply(expr.expr, ::apply), apply(expr.type, ::apply))

    companion object {
        val empty = Substitution(hashMap())
    }
}


sealed class TypeError {
    data class Unification(val ty1: Type, val ty2: Type) : TypeError()
    data class UnknownVar(val ident: Ident) : TypeError()
    data class OccursCheck(val ident: Ident, val type: Type) : TypeError()
    data class IfCondition(val type: Type) : TypeError()

    fun pretty(): String = when (this) {
        is Unification -> "Failed to unify ${ty1.pretty()} with ${ty2.pretty()}"
        is UnknownVar -> "Unknown var ${ident.ident}"
        is OccursCheck -> "Failed to infer the infinite type ${ident.ident} ~ ${type.pretty()}"
        is IfCondition -> "Condition should be of type Bool but was ${type.pretty()}"
    }
}

private val initialContext: TCContext = hashMap(
    Ident("add") to Scheme(
        emptyList(),
        Type.Fun(Type.Int.withDummySpan(), Type.Fun(Type.Int.withDummySpan(), Type.Int.withDummySpan()).withDummySpan())
    ),
    Ident("sub") to Scheme(
        emptyList(),
        Type.Fun(Type.Int.withDummySpan(), Type.Fun(Type.Int.withDummySpan(), Type.Int.withDummySpan()).withDummySpan())
    ),
    Ident("eq") to Scheme(
        emptyList(),
        Type.Fun(
            Type.Int.withDummySpan(),
            Type.Fun(Type.Int.withDummySpan(), Type.Bool.withDummySpan()).withDummySpan()
        )
    ),
    Ident("identity") to Scheme(
        listOf(Ident("a")),
        Type.Fun(Type.v("a").withDummySpan(), Type.v("a").withDummySpan())
    ), // forall a. a -> a
    Ident("fix") to Scheme(
        listOf(Ident("a")),
        Type.Fun(
            Type.Fun(Type.v("a").withDummySpan(), Type.v("a").withDummySpan()).withDummySpan(),
            Type.v("a").withDummySpan()
        )
    ) // forall a. (a -> a) -> a
)

class Typechecker {

    var fresh: Int = 0
    var errors = mutableListOf<Spanned<TypeError>>()

    fun freshVar(): Type {
        fresh += 1
        return Type.Var(Ident("u$fresh"))
    }

    fun reportError(error: TypeError, span: Span) {
        errors.add(Spanned(span, error))
    }

    fun instantiate(scheme: Scheme): Type {
        val x = scheme.vars.map { it to freshVar() }.toTypedArray()
        val s = Substitution(hashMap(*x))

        return s.apply(scheme.ty)
    }

    fun generalize(type: Type, ctx: TCContext): Scheme { // TODO clean up names
        val freeInCtx = ctx.values().map(Scheme::freeVars).foldLeft(hashSet<Ident>(), { a, b -> b.union(a) })
        val freeVars = type.freeVars().removeAll(freeInCtx)
        /*val vars = ('a'..'z').take(freeVars.size()).map { Ident(it.toString()) } */

        return Scheme(freeVars.toJavaList(), type)
    }

    fun unify(st1: Spanned<Type>, st2: Spanned<Type>): Either<TypeError, Substitution> {
        val t1 = st1.value
        val t2 = st2.value

        return if (t1 == t2)
            Either.right(Substitution.empty)
        else if (t1 is Type.Var)
            varBind(t1, t2)
        else if (t2 is Type.Var)
            varBind(t2, t1)
        else if (t1 is Type.Fun && t2 is Type.Fun) {
            unify(t1.arg, t2.arg).flatMap { s1 ->
                unify(s1.apply(t1.result, s1::apply), s1.apply(t2.result, s1::apply)).map { s2 ->
                    s2.compose(s1)
                }
            }
        } else {
            Either.left(TypeError.Unification(t1, t2) as TypeError)
        }
    }

    private fun varBind(v: Type.Var, type: Type): Either<TypeError, Substitution> {
        return if (type.freeVars().contains(v.ident))
            Either.left(TypeError.OccursCheck(v.ident, type))
        else
            Either.right(Substitution(hashMap(v.ident to type)))
    }

    fun infer(ctx: TCContext, sexpr: Spanned<Expression>): Pair<Spanned<Expression.Typed>, Substitution> {
        val (span, expr) = sexpr
        val errorSentinel = Type.ErrorSentinel.withDummySpan()

        return when (expr) {
            is Expression.Literal -> {
                val t = when (expr.lit) {
                    is IntLit -> Type.Int
                    is BoolLit -> Type.Bool
                }

                Expression.Typed(sexpr, t.withDummySpan()).withSpan(span) to Substitution.empty
            }
            is Expression.Lambda -> {
                val tyBinder = freshVar()
                val tmpCtx = ctx.put(expr.binder.value, Scheme(emptyList(), tyBinder))
                val (body, s) = this.infer(tmpCtx, expr.body)
                if (body.value.type.value.isError()) {
                    Expression.Typed(
                        Expression.Lambda(expr.binder, body).withSpan(span),
                        errorSentinel
                    ).withSpan(span) to Substitution.empty
                } else
                    s.apply(
                        Expression.Typed(
                            Expression.Lambda(expr.binder, body).withSpan(span),
                            Type.Fun(tyBinder.withDummySpan(), body.value.type).withDummySpan()
                        )
                    ).withSpan(span) to s
            }
            is Expression.Var -> {
                val scheme = ctx.get(expr.ident)
                if (scheme.isDefined) {
                    val t = instantiate(scheme.get()).withDummySpan()
                    Expression.Typed(sexpr, t).withSpan(span) to Substitution.empty
                } else {
                    reportError(TypeError.UnknownVar(expr.ident), span)
                    Expression.Typed(sexpr, errorSentinel).withSpan(span) to Substitution.empty
                }
            }
            is Expression.App -> {
                val tyRes = freshVar().withDummySpan()
                val (func, s1) = infer(ctx, expr.func)
                val (arg, s2) = infer(s1.apply(ctx), expr.arg)

                if (func.value.type.value.isError() || arg.value.type.value.isError()) {
                    Expression.Typed(
                        Expression.App(func, arg).withSpan(span),
                        errorSentinel
                    ).withSpan(span) to Substitution.empty
                } else {
                    val s3 =
                        unify(s2.apply(func.value.type, s2::apply), Type.Fun(arg.value.type, tyRes).withDummySpan())
                    s3.fold({ err ->
                        reportError(err, arg.span)
                        Expression.Typed(
                            Expression.App(func, arg).withSpan(span),
                            errorSentinel
                        ).withSpan(span) to Substitution.empty
                    }, { s3 ->
                        val s = s3.compose(s2).compose(s1)
                        s.apply(Expression.Typed(Expression.App(func, arg).withSpan(span), tyRes)).withSpan(span) to s
                    })
                }
            }
            is Expression.Typed -> {
                val (tyExpr, s) = infer(ctx, expr.expr)
                if (tyExpr.value.type.value.isError()) {
                    Expression.Typed(tyExpr, errorSentinel).withSpan(span) to Substitution.empty
                } else {
                    if (!expr.type.value.freeVars().isEmpty) {
                        throw RuntimeException("not allowed")
                    }
                    val s2 = unify(tyExpr.value.type, expr.type)
                    s2.fold({ err ->
                        reportError(err, span)
                        Expression.Typed(tyExpr, errorSentinel).withSpan(span) to Substitution.empty
                    }, { s2 ->
                        s2.apply(tyExpr, s2::apply) to s2.compose(s)
                    })
                }
            }
            is Expression.Let -> {
                val (tyBinder, s1) = infer(ctx, expr.expr)
                val genBinder = generalize(tyBinder.value.type.value, s1.apply(ctx))
                val tmpCtx = s1.apply(ctx.put(expr.binder.value, genBinder))
                val (tyBody, s2) = infer(tmpCtx, s1.apply(expr.body, s1::apply))

                val s = s2.compose(s1)

                s.apply(
                    Expression.Typed(
                        Expression.Let(expr.binder, tyBinder, tyBody).withSpan(span),
                        tyBody.value.type
                    )
                ).withSpan(span) to s
            }
            is Expression.If -> {
                var hasError = false
                val (tyCond, s1) = infer(ctx, expr.condition)

                val s2 = unify(tyCond.value.type, Type.Bool.withDummySpan()).fold(
                    {
                        reportError(TypeError.IfCondition(tyCond.value.type.value), expr.condition.span)
                        hasError = true
                        Substitution.empty
                    },
                    { s -> s }
                )

                val ctx = s2.apply(s1.apply(ctx))
                val (tyThen, s3) = infer(ctx, expr.thenBranch)
                val (tyElse, s4) = infer(s3.apply(ctx), expr.elseBranch)

                val s5 = unify(s4.apply(tyThen.value.type, s4::apply), tyElse.value.type).fold({ err ->
                    reportError(err, span)
                    hasError = true
                    Substitution.empty
                }, { s -> s })

                val s = s5.compose(s4).compose(s3).compose(s2).compose(s1)

                val type = if (hasError)
                    errorSentinel
                else
                    s.apply(tyThen.value.type, s::apply)

                s.apply(
                    Expression.Typed(
                        Expression.If(tyCond, tyThen, tyElse).withSpan(span),
                        type
                    )
                ).withSpan(span) to s
            }
        }
    }

    fun inferExpr(expr: Spanned<Expression>): Scheme {
        val (t, s) = this.infer(initialContext, expr)
        if (this.errors.isNotEmpty()) {
            this.errors.forEach {
                println("error: ${it.value.pretty()} ${if (it.span == Span.DUMMY) "" else it.span.toString()}")
            }
            println("inferred AST: ${t.value.pretty()}")
            throw RuntimeException("type errors occurred")
        }
        println("inferred AST: ${t.value.pretty()}")
        return generalize(s.apply(t.value.type.value), initialContext)
    }
}