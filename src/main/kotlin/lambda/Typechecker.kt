package lambda

// const = \x. \y. x : forall a b. a -> b -> a

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
            is Type.Var -> subst[ty.ident] ?: ty
        }
    }

    fun applyScheme(subst: Substitution, scheme: Scheme): Scheme {
        var tmpSubst = HashMap(subst)
        scheme.vars.forEach {
            tmpSubst.remove(it)
        }
        return Scheme(scheme.vars, applyType(tmpSubst, scheme.ty))
    }

    fun applyContext(subst: Substitution, ctx: TCContext): TCContext {
        return HashMap(ctx.mapValues {
            applyScheme(subst, it.component2())
        })
    }

    fun infer(ctx: TCContext, expr: Expression): Pair<Substitution, Type> {
        return when (expr) {
            is Literal -> {
                return when (expr.lit) {
                    is IntLit -> Pair(HashMap(), Type.Int)
                    is BoolLit -> Pair(HashMap(), Type.Bool)
                }
            }
            is Lambda -> {
                val tyBinder = freshVar()
                val tmpCtx = HashMap(ctx)
                tmpCtx.put(expr.binder, Scheme(emptyList(), tyBinder))
                val (s1, tyBody) = this.infer(tmpCtx, expr.body)
                return Pair(s1, Type.Fun(applyType(s1, tyBinder), tyBody))
            }
            else -> Pair(HashMap(), Type.Int)
        }
    }
}