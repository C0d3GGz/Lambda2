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

    fun apply(expr: Expression): Expression {
        return when (expr) {
            is Expression.Literal, is Expression.Var -> expr
            is Expression.Lambda -> Expression.Lambda(expr.binder, apply(expr.body))
            is Expression.App -> Expression.App(apply(expr.func), apply(expr.arg))
            is Expression.Typed -> apply(expr)
        }
    }

    fun apply(expr: Expression.Typed): Expression.Typed = Expression.Typed(apply(expr.expr), apply(expr.type))

    companion object {
        val empty = Substitution(hashMap())
    }
}


sealed class TypeError {
    data class Unification(val ty1: Type, val ty2: Type) : TypeError()
    data class UnknownVar(val ident: Ident) : TypeError()
    data class OccursCheck(val ident: Ident, val type: Type) : TypeError()

    fun pretty(): String = when (this) {
        is Unification -> "Failed to unify ${ty1.pretty()} with ${ty2.pretty()}"
        is UnknownVar -> "Unknown var ${ident.ident}"
        is OccursCheck -> "Failed to infer the infinite type ${ident.ident} ~ ${type.pretty()}"
    }
}

private val initialContext: TCContext = hashMap(
    Ident("add") to Scheme(emptyList(), Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Int))),
    Ident("identity") to Scheme(listOf(Ident("a")), Type.Fun(Type.v("a"), Type.v("a"))) // forall a. a -> a
)

class Typechecker {

    var fresh: Int = 0
    var errors = mutableListOf<TypeError>()

    fun freshVar(): Type {
        fresh += 1
        return Type.Var(Ident("u$fresh"))
    }

    fun reportError(error: TypeError) {
        errors.add(error)
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

    fun unify(t1: Type, t2: Type): Either<TypeError, Substitution> {
        return if (t1 == t2)
            Either.right(Substitution.empty)
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
            Either.left(TypeError.Unification(t1, t2) as TypeError)
        }
    }

    private fun varBind(v: Type.Var, type: Type): Either<TypeError, Substitution> {
        return if (type.freeVars().contains(v.ident))
            Either.left(TypeError.OccursCheck(v.ident, type))
        else
            Either.right(Substitution(hashMap(v.ident to type)))
    }

    fun infer(ctx: TCContext, expr: Expression): Pair<Expression.Typed, Substitution> {
        return when (expr) {
            is Expression.Literal -> {
                val t = when (expr.lit) {
                    is IntLit -> Type.Int
                    is BoolLit -> Type.Bool
                }

                Expression.Typed(expr, t) to Substitution.empty
            }
            is Expression.Lambda -> {
                val tyBinder = freshVar()
                val tmpCtx = ctx.put(expr.binder, Scheme(emptyList(), tyBinder))
                val (body, s) = this.infer(tmpCtx, expr.body)
                if (body.type.isError()) {
                    Expression.Typed(Expression.Lambda(expr.binder, body), Type.ErrorSentinel) to Substitution.empty
                } else
                    s.apply(Expression.Typed(Expression.Lambda(expr.binder, body), Type.Fun(tyBinder, body.type))) to s
            }
            is Expression.Var -> {
                val scheme = ctx.get(expr.ident)
                if (scheme.isDefined) {
                    val t = instantiate(scheme.get())
                    Expression.Typed(expr, t) to Substitution.empty
                } else {
                    reportError(TypeError.UnknownVar(expr.ident))
                    Expression.Typed(expr, Type.ErrorSentinel) to Substitution.empty
                }
            }
            is Expression.App -> {
                val tyRes = freshVar()
                val (func, s1) = infer(ctx, expr.func)
                val (arg, s2) = infer(s1.apply(ctx), expr.arg)

                if (func.type.isError() || arg.type.isError()) {
                    Expression.Typed(Expression.App(func, arg), Type.ErrorSentinel) to Substitution.empty
                } else {
                    val s3 = unify(s2.apply(func.type), Type.Fun(arg.type, tyRes))
                    s3.fold({ err ->
                        reportError(err)
                        Expression.Typed(Expression.App(func, arg), Type.ErrorSentinel) to Substitution.empty
                    }, { s3 ->
                        val s = s3.compose(s2).compose(s1)
                        s.apply(Expression.Typed(Expression.App(func, arg), tyRes)) to s
                    })
                }

            }
            is Expression.Typed -> {
                val (tyExpr, s) = infer(ctx, expr.expr)
                if (tyExpr.type.isError()) {
                    Expression.Typed(tyExpr, Type.ErrorSentinel) to Substitution.empty
                } else {
                    if (!expr.type.freeVars().isEmpty) {
                        throw RuntimeException("not allowed")
                    }
                    val s2 = unify(tyExpr.type, expr.type)
                    s2.fold({ err ->
                        reportError(err)
                        Expression.Typed(tyExpr, Type.ErrorSentinel) to Substitution.empty
                    }, { s2 ->
                        s2.apply(tyExpr) to s2.compose(s)
                    })
                }
            }
        }
    }

    fun inferExpr(expr: Expression): Scheme {
        val (t, s) = this.infer(initialContext, expr)
        if (this.errors.isNotEmpty()) {
            this.errors.forEach {
                println("error: ${it.pretty()}")
            }
            println("inferred AST: ${t.pretty()}")
            throw RuntimeException("type errors occurred")
        }
        println("inferred AST: ${t.pretty()}")
        return generalize(s.apply(t.type), initialContext)
    }
}