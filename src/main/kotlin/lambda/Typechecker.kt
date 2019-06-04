package lambda

import io.vavr.kotlin.*
import io.vavr.collection.HashMap

typealias TCContext = HashMap<Ident, Scheme>
typealias Substitution = HashMap<Ident, Type>

class Typechecker {

    var fresh: Int = 0

    fun freshVar(): Type {
        fresh += 1
        return Type.Var(Ident(fresh.toString()))
    }

    fun applyType(subst: Substitution, ty: Type): Type {
        return when (ty) {
            is Type.Int -> ty
            is Type.Bool -> ty
            is Type.Fun -> Type.Fun(applyType(subst, ty.arg), applyType(subst, ty.result))
            is Type.Var -> subst.get(ty.ident).getOrElse(ty)
        }
    }

    fun applyScheme(subst: Substitution, scheme: Scheme): Scheme {
        val tmpSubst = subst.removeAll(scheme.vars)
        return Scheme(scheme.vars, applyType(tmpSubst, scheme.ty))
    }

    fun applyContext(subst: Substitution, ctx: TCContext): TCContext {
        return ctx.mapValues {
            applyScheme(subst, it)
        }
    }

    fun infer(ctx: TCContext, expr: Expression): Pair<Substitution, Type> {
        return when (expr) {
            is Literal -> {
                return when (expr.lit) {
                    is IntLit -> Pair(hashMap(), Type.Int)
                    is BoolLit -> Pair(hashMap(), Type.Bool)
                }
            }
            is Lambda -> {
                val tyBinder = freshVar()
                val tmpCtx = ctx.put(expr.binder, Scheme(emptyList(), tyBinder))
                val (s1, tyBody) = this.infer(tmpCtx, expr.body)
                return Pair(s1, Type.Fun(applyType(s1, tyBinder), tyBody))
            }
            else -> Pair(hashMap(), Type.Int)
        }
    }
}