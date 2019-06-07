package lambda

import io.vavr.kotlin.*
import io.vavr.collection.HashMap
import io.vavr.control.Option

typealias TCContext = HashMap<Ident, Scheme>

class Substitution(val subst: HashMap<Ident, Type>) {

    fun get(ident: Ident): Option<Type> {
        return subst.get(ident)
    }

    fun apply(ty: Type): Type {
        return when (ty) {
            is Type.Int -> ty
            is Type.Bool -> ty
            is Type.Fun -> Type.Fun(this.apply(ty.arg), this.apply(ty.result))
            is Type.Var -> this.get(ty.ident).getOrElse(ty)
        }
    }

//    fun removeAll(idents: List<Ident>): Substitution {
//        return Substitution(subst.removeAll(idents))
//    }
//
//    fun apply(scheme: Scheme): Scheme {
//        val tmpSubst = Substitution(this.subst.removeAll(scheme.vars))
//        return Scheme(scheme.vars, tmpSubst.apply(scheme.ty))
//    }
//
//    fun apply(ctx: TCContext): TCContext {
//        return ctx.mapValues {
//            this.apply(it)
//        }
//    }

}

val emptySubstitution = Substitution(hashMap())

class Typechecker {

    var fresh: Int = 0

    fun freshVar(): Type {
        fresh += 1
        return Type.Var(Ident(fresh.toString()))
    }

    fun infer(ctx: TCContext, expr: Expression): Pair<Substitution, Type> {
        return when (expr) {
            is Literal -> {
                return when (expr.lit) {
                    is IntLit -> Pair(emptySubstitution, Type.Int)
                    is BoolLit -> Pair(emptySubstitution, Type.Bool)
                }
            }
            is Lambda -> {
                val tyBinder = freshVar()
                val tmpCtx = ctx.put(expr.binder, Scheme(emptyList(), tyBinder))
                val (s1, tyBody) = this.infer(tmpCtx, expr.body)
                return Pair(s1, Type.Fun(s1.apply(tyBinder), tyBody))
            }
            else -> Pair(emptySubstitution, Type.Int)
        }
    }
}