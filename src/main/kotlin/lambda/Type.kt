package lambda

import io.vavr.collection.HashSet
import io.vavr.kotlin.hashSet
import lambda.syntax.Name

sealed class Type {
    object Int : Type()
    object Bool : Type()
    object ErrorSentinel : Type()
    data class Var(val name: Name) : Type()
    data class Fun(val arg: Spanned<Type>, val result: Spanned<Type>) : Type()

    fun isError() = this is ErrorSentinel

    fun freeVars(): HashSet<Name> {
        return when (this) {
            Int, Bool, ErrorSentinel -> hashSet()
            is Var -> hashSet(name)
            is Fun -> arg.value.freeVars().union(result.value.freeVars())
        }
    }

    fun withDummySpan() = Spanned(Span.DUMMY, this)

    companion object {
        fun v(name: String) = Type.Var(Name(name))
    }
}

data class Scheme(val vars: List<Name>, val ty: Type) {

    fun freeVars(): HashSet<Name> = ty.freeVars().removeAll(vars)
}