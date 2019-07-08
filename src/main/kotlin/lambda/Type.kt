package lambda

import io.vavr.collection.HashSet
import io.vavr.kotlin.hashSet

sealed class Type {
    object Int : Type()
    object Bool : Type()
    object ErrorSentinel : Type()
    data class Var(val ident: Ident) : Type()
    data class Fun(val arg: Type, val result: Type) : Type()

    fun isError() = this is ErrorSentinel

    fun freeVars(): HashSet<Ident> {
        return when (this) {
            Int, Bool, ErrorSentinel -> hashSet()
            is Var -> hashSet(ident)
            is Fun -> arg.freeVars().union(result.freeVars())
        }
    }

    companion object {
        fun v(ident: String) = Type.Var(Ident(ident))
    }
}

data class Scheme(val vars: List<Ident>, val ty: Type) {

    fun freeVars(): HashSet<Ident> = ty.freeVars().removeAll(vars)
}