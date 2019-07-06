package lambda

import io.vavr.collection.HashMap
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
            Type.Int -> Type.Int
            Type.Bool -> Type.Bool
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

    companion object {
        val empty = Substitution(hashMap())
    }
}

fun unify(t1: Type, t2: Type): Substitution {
    return if (t1 == t2)
        Substitution.empty
    else if (t1 is Type.Var)
        varBind(t1, t2)
    else if (t2 is Type.Var)
        varBind(t2, t1)
    else if (t1 is Type.Fun && t2 is Type.Fun) {
        val s1 = unify(t1.arg, t2.arg)
        val s2 = unify(s1.apply(t1.result), s1.apply(t2.result))

        s1.compose(s2)
    }
    else
        throw RuntimeException("failed to unify ${t1.pretty()} with ${t2.pretty()}")
}

private fun varBind(v: Type.Var, type: Type): Substitution {
    return if (type.freeVars().contains(v.ident))
        throw RuntimeException("failed to infer the infinite type ${v.ident.ident} ~ ${type.pretty()}")
    else
        Substitution(hashMap(v.ident to type))
}

private val initialContext: TCContext = hashMap(
    Ident("add") to Scheme(emptyList(), Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Int))),
    Ident("identity") to Scheme(listOf(Ident("a")), Type.Fun(Type.v("a"), Type.v("a"))) // forall a. a -> a
)

class Typechecker {

    var fresh: Int = 0

    fun freshVar(): Type {
        fresh += 1
        return Type.Var(Ident("u${fresh.toString()}"))
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

    fun infer(ctx: TCContext, expr: Expression): Pair<Type, Substitution> {
        return when (expr) {
            is Expression.Literal -> {
                val t = when (expr.lit) {
                    is IntLit -> Type.Int
                    is BoolLit -> Type.Bool
                }

                t to Substitution.empty
            }
            is Expression.Lambda -> {
                val tyBinder = freshVar()
                val tmpCtx = ctx.put(expr.binder, Scheme(emptyList(), tyBinder))
                val (tyBody, s) = this.infer(tmpCtx, expr.body)
                Type.Fun(s.apply(tyBinder), tyBody) to s
            }
            is Expression.Var -> {
                val scheme = ctx.get(expr.ident)
                if (scheme.isDefined) instantiate(scheme.get()) to Substitution.empty else throw RuntimeException("")
            }
            is Expression.App -> {
                val tyRes = freshVar()
                val (tyFun, s1) = infer(ctx, expr.func)
                val (tyArg, s2) = infer(s1.apply(ctx), expr.arg)

                val s3 = unify(s2.apply(tyFun), Type.Fun(tyArg, tyRes))
                val s = s1.compose(s2).compose(s3)

                s.apply(tyRes) to s
            }
        }
    }

    fun inferExpr(expr: Expression): Scheme {
        val (t, s) = this.infer(initialContext, expr)
        return generalize(s.apply(t), initialContext)
    }
}