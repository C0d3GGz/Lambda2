package lambda

import io.vavr.collection.HashSet
import io.vavr.kotlin.hashSet
import lambda.syntax.Name

inline class TyVar(val name: Name) {
    override fun toString(): String = name.toString()
}

sealed class Type {
    val span: Span
        get() = when (this) {
            ErrorSentinel -> Span.DUMMY
            is Constructor -> name.span
            is Var -> v.name.span
            is Fun -> sp
            is Unknown -> Span.DUMMY
        }

    object ErrorSentinel : Type()
    data class Constructor(val name: Name, val tyArgs: List<Type> = emptyList()) : Type()
    data class Var(val v: TyVar) : Type()
    data class Unknown(val u: Int) : Type() {
        override fun toString(): String = "u$u"
    }

    data class Fun(val arg: Type, val result: Type, val sp: Span = Span.DUMMY) : Type()

    fun isError() = this is ErrorSentinel

    fun freeVars(): HashSet<TyVar> {
        return when (this) {
            is Constructor, ErrorSentinel -> hashSet()
            is Var -> hashSet(v)
            is Fun -> arg.freeVars().union(result.freeVars())
            is Unknown -> hashSet()
        }
    }

    fun unknowns(): HashSet<Int> {
        return when (this) {
            is ErrorSentinel -> hashSet()
            is Constructor -> tyArgs.fold(hashSet(), { acc, arg -> acc.union(arg.unknowns()) })
            is Var -> hashSet()
            is Fun -> arg.unknowns().union(result.unknowns())
            is Unknown -> hashSet(u)
        }
    }

    fun subst(tyVar: TyVar, type: Type): Type {
        return when (this) {
            is Var -> if (v == tyVar) type else this
            is Fun -> Fun(arg.subst(tyVar, type), result.subst(tyVar, type), sp)
            is Constructor -> Type.Constructor(name, tyArgs.map { it.subst(tyVar, type) })
            ErrorSentinel, is Unknown -> this
        }
    }

    fun substMany(subst: List<Pair<TyVar, Type>>): Type =
        subst.fold(this) { acc, (v, t) -> acc.subst(v, t) }

    companion object {
        fun v(name: String) = Var(TyVar(Name(name)))
        val Int = Constructor(Name("Int"))
        val Bool = Constructor(Name("Bool"))
        val String = Constructor(Name("String"))
    }
}

data class Scheme(val vars: List<TyVar>, val ty: Type) {
    val span: Span get() = Span(vars.firstOrNull()?.name?.span?.start ?: ty.span.start, ty.span.end)
    fun freeVars(): HashSet<TyVar> = ty.freeVars().removeAll(vars)
    fun unknowns(): HashSet<Int> = ty.unknowns()

    companion object {
        fun fromType(type: Type): Scheme = Scheme(emptyList(), type)
    }
}