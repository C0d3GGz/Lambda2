package lambda

import io.vavr.collection.HashMap
import io.vavr.control.Option
import io.vavr.kotlin.hashMap

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
    Ident("add") to Scheme(emptyList(), Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Int)))
)

class Typechecker {

    var fresh: Int = 0

    fun freshVar(): Type {
        fresh += 1
        return Type.Var(Ident("u${fresh.toString()}"))
    }

    fun infer(ctx: TCContext, expr: Expression): Type {
        return when (expr) {
            is Literal -> {
                return when (expr.lit) {
                    is IntLit -> Type.Int
                    is BoolLit -> Type.Bool
                }
            }
            is Lambda -> {
                val tyBinder = freshVar()
                val tmpCtx = ctx.put(expr.binder, Scheme(emptyList(), tyBinder))
                val tyBody = this.infer(tmpCtx, expr.body)
                return Type.Fun(tyBinder, tyBody)
            }
            else -> throw Exception("Unmatched typechecker case")
        }
    }

    fun inferExpr(expr: Expression) = this.infer(initialContext, expr)
}