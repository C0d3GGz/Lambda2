package lambda

import io.vavr.collection.HashMap
import io.vavr.control.Option
import io.vavr.kotlin.hashMap
import io.vavr.kotlin.hashSet
import lambda.syntax.*

typealias TCContext = HashMap<Name, Scheme>

data class Substitution(val subst: HashMap<Name, Type>) {
    fun get(name: Name): Option<Type> = subst.get(name)

    fun compose(that: Substitution): Substitution {
        return Substitution(that.subst.mapValues(::apply).merge(subst))
    }

    fun apply(type: Type): Type {
        return when (type) {
            is Type.Constructor, Type.ErrorSentinel -> type
            is Type.Var -> get(type.name).getOrElse(type)
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

    fun apply(case: Expression.Case): Expression.Case {
        return case.copy(body = apply(case.body, ::apply))
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
            is Expression.Construction -> expr.copy(exprs = expr.exprs.map { apply(it, ::apply) })
            is Expression.Match -> Expression.Match(apply(expr.expr, ::apply), expr.cases.map(::apply))
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
    data class UnknownVar(val name: Name) : TypeError()
    data class UnknownType(val name: Name) : TypeError()
    data class UnknownDtor(val type: Name, val name: Name) : TypeError()
    data class OccursCheck(val name: Name, val type: Type) : TypeError()
    data class IfCondition(val type: Type) : TypeError()

    fun pretty(): String = when (this) {
        is Unification -> "Failed to unify ${ty1.pretty()} with ${ty2.pretty()}"
        is UnknownVar -> "Unknown var ${name.value}"
        is UnknownType -> "Unknown type ${name.value}"
        is UnknownDtor -> "Type ${type.value} does not have a constructor named ${name.value}"
        is OccursCheck -> "Failed to infer the infinite type ${name.value} ~ ${type.pretty()}"
        is IfCondition -> "Condition should be of type Bool but was ${type.pretty()}"
    }
}

private val initialContext: TCContext
    get() {
        val list = Type.Constructor(Spanned(Span.DUMMY, Name("List"))).withDummySpan()

        return hashMap(
            Name("add") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.Int.withDummySpan(),
                    Type.Fun(Type.Int.withDummySpan(), Type.Int.withDummySpan()).withDummySpan()
                )
            ),
            Name("sub") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.Int.withDummySpan(),
                    Type.Fun(Type.Int.withDummySpan(), Type.Int.withDummySpan()).withDummySpan()
                )
            ),
            Name("eq") to Scheme(
                emptyList(),
                Type.Fun(
                    Type.Int.withDummySpan(),
                    Type.Fun(Type.Int.withDummySpan(), Type.Bool.withDummySpan()).withDummySpan()
                )
            ),
            Name("identity") to Scheme(
                listOf(Name("a")),
                Type.Fun(Type.v("a").withDummySpan(), Type.v("a").withDummySpan())
            ), // forall a. a -> a
            Name("fix") to Scheme(
                listOf(Name("a")),
                Type.Fun(
                    Type.Fun(Type.v("a").withDummySpan(), Type.v("a").withDummySpan()).withDummySpan(),
                    Type.v("a").withDummySpan()
                )
            )
        )
    }

class Typechecker {

    var fresh: Int = 0
    var errors = mutableListOf<Spanned<TypeError>>()

    val types: MutableMap<Name, List<DataConstructor>> = mutableMapOf(
        Name("Int") to emptyList(),
        Name("Bool") to emptyList()
    )

    fun freshVar(): Type {
        fresh += 1
        return Type.Var(Name("u$fresh"))
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
        val freeInCtx = ctx.values().map(Scheme::freeVars).foldLeft(hashSet<Name>(), { a, b -> b.union(a) })
        val freeVars = type.freeVars().removeAll(freeInCtx)
        /*val vars = ('a'..'z').take(freeVars.size()).map { Name(it.toString()) } */

        return Scheme(freeVars.toJavaList(), type)
    }

    fun unify(st1: Spanned<Type>, st2: Spanned<Type>): Either<TypeError, Substitution> {
        val t1 = st1.value
        val t2 = st2.value

        return if (t1 == t2)
            Either.Right(Substitution.empty)
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
            Either.Left(TypeError.Unification(t1, t2) as TypeError)
        }
    }

    private fun varBind(v: Type.Var, type: Type): Either<TypeError, Substitution> {
        return if (type.freeVars().contains(v.name))
            Either.Left(TypeError.OccursCheck(v.name, type))
        else
            Either.Right(Substitution(hashMap(v.name to type)))
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
                val scheme = ctx.get(expr.name)
                if (scheme.isDefined) {
                    val t = instantiate(scheme.get()).withDummySpan()
                    Expression.Typed(sexpr, t).withSpan(span) to Substitution.empty
                } else {
                    reportError(TypeError.UnknownVar(expr.name), span)
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
                    when (val res =
                        unify(s2.apply(func.value.type, s2::apply), Type.Fun(arg.value.type, tyRes).withDummySpan())) {
                        is Either.Left -> {
                            reportError(res.value, arg.span)
                            Expression.Typed(
                                Expression.App(func, arg).withSpan(span),
                                errorSentinel
                            ).withSpan(span) to Substitution.empty
                        }
                        is Either.Right -> {
                            val s = res.value.compose(s2).compose(s1)
                            s.apply(
                                Expression.Typed(
                                    Expression.App(func, arg).withSpan(span),
                                    tyRes
                                )
                            ).withSpan(span) to s
                        }
                    }
                }
            }
            is Expression.Typed -> {
                val (tyExpr, s) = infer(ctx, expr.expr)
                if (tyExpr.value.type.value.isError()) {
                    Expression.Typed(tyExpr, errorSentinel).withSpan(span) to Substitution.empty
                } else {
                    if (!expr.type.value.freeVars().isEmpty) { // TODO check wellformedness
                        throw RuntimeException("not allowed")
                    }
                    when (val res = unify(tyExpr.value.type, expr.type)) {
                        is Either.Left -> {
                            reportError(res.value, span)
                            Expression.Typed(tyExpr, errorSentinel).withSpan(span) to Substitution.empty
                        }
                        is Either.Right -> {
                            val s2 = res.value
                            s2.apply(tyExpr, s2::apply) to s2.compose(s)
                        }
                    }
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

                val s2 = when (val res = unify(tyCond.value.type, Type.Bool.withDummySpan())) {
                    is Either.Left -> {
                        reportError(TypeError.IfCondition(tyCond.value.type.value), expr.condition.span)
                        hasError = true
                        Substitution.empty
                    }
                    is Either.Right -> res.value
                }

                val ctx = s2.apply(s1.apply(ctx))
                val (tyThen, s3) = infer(ctx, expr.thenBranch)
                val (tyElse, s4) = infer(s3.apply(ctx), expr.elseBranch)

                val s5 = when (val res = unify(s4.apply(tyThen.value.type, s4::apply), tyElse.value.type)) {
                    is Either.Left -> {
                        reportError(res.value, span)
                        hasError = true
                        Substitution.empty
                    }
                    is Either.Right -> res.value
                }

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

            is Expression.Construction -> when (val res = lookupDtor(expr.type.value, expr.dtor.value)) {
                is Either.Left -> {
                    reportError(res.value, Span(expr.type.span.start, expr.dtor.span.end))
                    Expression.Typed(expr.withSpan(span), errorSentinel).withSpan(span) to Substitution.empty
                }
                is Either.Right -> {
                    val fields = res.value
                    var hasError = false
                    var typedFields = mutableListOf<Spanned<Expression.Typed>>()
                    var s = Substitution.empty

                    expr.exprs.zip(fields).forEach { (e, f) ->
                        val (t, s1) = infer(ctx, s.apply(e, s::apply))

                        when (val res = unify(t.value.type, f.withDummySpan())) {
                            is Either.Left -> {
                                reportError(res.value, e.span)
                                hasError = true
                                typedFields.add(t)
                            }
                            is Either.Right -> {
                                s = res.value.compose(s1).compose(s)
                                typedFields.add(s.apply(t, s::apply))
                            }
                        }
                    }

                    val type = if (hasError) {
                        errorSentinel
                    } else {
                        Type.Constructor(expr.type).withDummySpan()
                    }

                    Expression.Typed(
                        Expression.Construction(expr.type, expr.dtor, typedFields).withSpan(span),
                        type
                    ).withSpan(span) to s
                }
            }
            is Expression.Match ->
                Expression.Typed(sexpr, errorSentinel).withSpan(span) to Substitution.empty
        }
    }

    fun lookupDtor(type: Name, dtor: Name): Either<TypeError, List<Type>> {
        val dtors = types[type] ?: return Either.Left(TypeError.UnknownType(type))
        val dc = dtors.find { it.name == dtor } ?: return Either.Left(TypeError.UnknownDtor(type, dtor))
        return Either.Right(dc.fields)
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

    fun inferSourceFile(file: SourceFile): HashMap<Name, Scheme> {
        var ctx = initialContext

        file.typeDeclarations().forEach {
            types.putIfAbsent(it.name.value, it.dataConstructors)
        }

        file.valueDeclarations().forEach {
            val (t, s) = this.infer(ctx, it.expr)
            // TODO compare user scheme with inferred scheme

            if (this.errors.isNotEmpty()) {
                this.errors.forEach {
                    println("error: ${it.value.pretty()} ${if (it.span == Span.DUMMY) "" else it.span.toString()}")
                }
                println("inferred AST: ${t.value.pretty()}")
                throw RuntimeException("type errors occurred")
            }
            println("inferred AST: ${t.value.pretty()}")
            val scheme = generalize(s.apply(t.value.type.value), ctx)
            ctx = ctx.put(it.name.value, scheme)
        }

        return ctx.removeAll(initialContext.keysIterator())
    }
}