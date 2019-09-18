package lambda

import io.vavr.collection.HashSet
import io.vavr.kotlin.hashSet
import lambda.syntax.Name

sealed class Type {
    val span: Span
        get() = when(this) {
            ErrorSentinel -> Span.DUMMY
            is Constructor -> name.span
            is Var -> name.span
            is Fun -> sp
        }

    object ErrorSentinel : Type()
    data class Constructor(val name: Name) : Type()
    data class Var(val name: Name) : Type()
    data class Fun(val arg: Type, val result: Type, val sp: Span = Span.DUMMY) : Type()

    fun isError() = this is ErrorSentinel

    fun freeVars(): HashSet<Name> {
        return when (this) {
            is Constructor, ErrorSentinel -> hashSet()
            is Var -> hashSet(name)
            is Fun -> arg.freeVars().union(result.freeVars())
        }
    }

    companion object {
        fun v(name: String) = Var(Name(name))
        val Int = Constructor(Name("Int"))
        val Bool = Constructor(Name("Bool"))
    }
}

data class Scheme(val vars: List<Name>, val ty: Type) {
    val span: Span get() = Span(vars.firstOrNull()?.span?.start ?: ty.span.start, ty.span.end)
    fun freeVars(): HashSet<Name> = ty.freeVars().removeAll(vars)
}