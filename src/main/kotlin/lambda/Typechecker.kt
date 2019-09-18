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
            is Type.Fun -> Type.Fun(apply(type.arg), apply(type.result))
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
                listOf(Name("a")),
                Type.Fun(Type.v("a"), Type.v("a"))
            ), // forall a. a -> a
            Name("fix") to Scheme(
                listOf(Name("a")),
                Type.Fun(
                    Type.Fun(Type.v("a"), Type.v("a")),
                    Type.v("a")
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

    fun unify(t1: Type, t2: Type): Either<TypeError, Substitution> {
        return if (t1 == t2)
            Either.Right(Substitution.empty)
        else if (t1 is Type.Var)
            varBind(t1, t2)
        else if (t2 is Type.Var)
            varBind(t2, t1)
        else if (t1 is Type.Fun && t2 is Type.Fun) {
            unify(t1.arg, t2.arg).flatMap { s1 ->
                unify(s1.apply(t1.result), s1.apply(t2.result)).map { s2 ->
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

    fun infer(ctx: TCContext, expr: Expression): Pair<Expression.Typed, Substitution> {
        val span = expr.span
        val errorSentinel = Type.ErrorSentinel

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
                val (body, s) = this.infer(tmpCtx, expr.body)
                if (body.type.isError()) {
                    tyWrap(
                        Expression.Lambda(expr.binder, body, span),
                        errorSentinel
                    ) to Substitution.empty
                } else
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
                    val t = instantiate(scheme.get())
                    tyWrap(expr, t) to Substitution.empty
                } else {
                    reportError(TypeError.UnknownVar(expr.name), span)
                    tyWrap(expr, errorSentinel) to Substitution.empty
                }
            }
            is Expression.App -> {
                val tyRes = freshVar()
                val (func, s1) = infer(ctx, expr.func)
                val (arg, s2) = infer(s1.apply(ctx), expr.arg)

                if (func.type.isError() || arg.type.isError()) {
                    tyWrap(Expression.App(func, arg, span), errorSentinel) to Substitution.empty
                } else {
                    when (val res =
                        unify(s2.apply(func.type), Type.Fun(arg.type, tyRes))) {
                        is Either.Left -> {
                            reportError(res.value, arg.span)
                            tyWrap(Expression.App(func, arg, span), errorSentinel) to Substitution.empty
                        }
                        is Either.Right -> {
                            val s = res.value.compose(s2).compose(s1)
                            s.apply(
                                tyWrap(Expression.App(func, arg, span), tyRes)
                            ) to s
                        }
                    }
                }
            }
            is Expression.Typed -> {
                val (tyExpr, s) = infer(ctx, expr.expr)
                if (tyExpr.type.isError()) {
                    tyWrap(tyExpr, errorSentinel) to Substitution.empty
                } else {
                    if (!expr.type.freeVars().isEmpty) { // TODO check wellformedness
                        throw RuntimeException("not allowed")
                    }
                    when (val res = unify(tyExpr.type, expr.type)) {
                        is Either.Left -> {
                            reportError(res.value, span)
                            tyWrap(tyExpr, errorSentinel) to Substitution.empty
                        }
                        is Either.Right -> {
                            val s2 = res.value
                            s2.apply(tyExpr) to s2.compose(s)
                        }
                    }
                }
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
                var hasError = false
                val (tyCond, s1) = infer(ctx, expr.condition)

                val s2 = when (val res = unify(tyCond.type, Type.Bool)) {
                    is Either.Left -> {
                        reportError(TypeError.IfCondition(tyCond.type), expr.condition.span)
                        hasError = true
                        Substitution.empty
                    }
                    is Either.Right -> res.value
                }

                val ctx = s2.apply(s1.apply(ctx))
                val (tyThen, s3) = infer(ctx, expr.thenBranch)
                val (tyElse, s4) = infer(s3.apply(ctx), expr.elseBranch)

                val s5 = when (val res = unify(s4.apply(tyThen.type), tyElse.type)) {
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
                    s.apply(tyThen.type)

                s.apply(tyWrap(Expression.If(tyCond, tyThen, tyElse, span), type)) to s
            }

            is Expression.Construction -> when (val res = lookupDtor(expr.type, expr.dtor)) {
                is Either.Left -> {
                    reportError(res.value, Span(expr.type.span.start, expr.dtor.span.end))
                    tyWrap(expr, errorSentinel) to Substitution.empty
                }
                is Either.Right -> {
                    val fields = res.value
                    var hasError = false
                    val typedFields = mutableListOf<Expression.Typed>()
                    var s = Substitution.empty

                    expr.exprs.zip(fields).forEach { (e, f) ->
                        val (t, s1) = infer(ctx, s.apply(e))

                        when (val res = unify(t.type, f)) {
                            is Either.Left -> {
                                reportError(res.value, e.span)
                                hasError = true
                                typedFields.add(t)
                            }
                            is Either.Right -> {
                                s = res.value.compose(s1).compose(s)
                                typedFields.add(s.apply(t))
                            }
                        }
                    }

                    val type = if (hasError) {
                        errorSentinel
                    } else {
                        Type.Constructor(expr.type)
                    }

                    tyWrap(Expression.Construction(expr.type, expr.dtor, typedFields, span), type) to s
                }
            }
            is Expression.Match ->
                tyWrap(expr, errorSentinel) to Substitution.empty
        }
    }

    fun lookupDtor(type: Name, dtor: Name): Either<TypeError, List<Type>> {
        val dtors = types[type] ?: return Either.Left(TypeError.UnknownType(type))
        val dc = dtors.find { it.name == dtor } ?: return Either.Left(TypeError.UnknownDtor(type, dtor))
        return Either.Right(dc.fields)
    }

    fun inferExpr(expr: Expression): Scheme {
        val (t, s) = this.infer(initialContext, expr)
        if (this.errors.isNotEmpty()) {
            this.errors.forEach {
                println("error: ${it.value.pretty()} ${if (it.span == Span.DUMMY) "" else it.span.toString()}")
            }
            println("inferred AST: ${t.pretty()}")
            throw RuntimeException("type errors occurred")
        }
        println("inferred AST: ${t.pretty()}")
        return generalize(s.apply(t.type), initialContext)
    }

    fun inferSourceFile(file: SourceFile): HashMap<Name, Scheme> {
        var ctx = initialContext

        file.typeDeclarations().forEach {
            types.putIfAbsent(it.name, it.dataConstructors)
        }

        file.valueDeclarations().forEach {
            val (t, s) = this.infer(ctx, it.expr)
            // TODO compare user scheme with inferred scheme

            if (this.errors.isNotEmpty()) {
                this.errors.forEach {
                    println("error: ${it.value.pretty()} ${if (it.span == Span.DUMMY) "" else it.span.toString()}")
                }
                println("inferred AST: ${t.pretty()}")
                throw RuntimeException("type errors occurred")
            }
            println("inferred AST: ${t.pretty()}")
            val scheme = generalize(s.apply(t.type), ctx)
            ctx = ctx.put(it.name, scheme)
        }

        return ctx.removeAll(initialContext.keysIterator())
    }
}