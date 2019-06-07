package lambda

import io.vavr.collection.HashMap
import io.vavr.control.Option
import io.vavr.kotlin.hashMap

typealias TCContext = HashMap<Ident, Scheme>

class Substitution(val subst: HashMap<Ident, Type>) {
    fun get(ident: Ident): Option<Type> = subst.get(ident)

    fun compose(other: Substitution): Substitution {
        TODO()
    }

    fun apply(type: Type): Type {
        return when (type) {
            Type.Int -> Type.Int
            Type.Bool -> Type.Bool
            is Type.Var -> get(type.ident).getOrElse(type)
            is Type.Fun -> Type.Fun(apply(type.arg), apply(type.result))
        }
    }

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