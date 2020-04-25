package lambda

import kotlinx.collections.immutable.PersistentSet
import kotlinx.collections.immutable.persistentHashSetOf
import kotlinx.collections.immutable.persistentSetOf
import lambda.syntax.Name
import lambda.syntax.Namespace

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
    data class Constructor(val namespace: Namespace, val name: Name, val tyArgs: List<Type> = emptyList()) : Type()
    data class Var(val v: TyVar) : Type()
    data class Unknown(val u: Int) : Type() {
        override fun toString(): String = "u$u"
    }

    data class Fun(val arg: Type, val result: Type, val sp: Span = Span.DUMMY) : Type()

    fun isError() = this is ErrorSentinel

    fun freeVars(): PersistentSet<TyVar> {
        return when (this) {
            is Constructor, ErrorSentinel -> persistentSetOf()
            is Var -> persistentSetOf(v)
            is Fun -> arg.freeVars().addAll(result.freeVars())
            is Unknown -> persistentSetOf()
        }
    }

    fun unknowns(): PersistentSet<Int> {
        return when (this) {
            is ErrorSentinel -> persistentSetOf()
            is Constructor -> tyArgs.fold(persistentSetOf(), { acc, arg -> acc.addAll(arg.unknowns()) })
            is Var -> persistentSetOf()
            is Fun -> arg.unknowns().addAll(result.unknowns())
            is Unknown -> persistentSetOf(u)
        }
    }

    fun subst(tyVar: TyVar, type: Type): Type {
        return when (this) {
            is Var -> if (v == tyVar) type else this
            is Fun -> Fun(arg.subst(tyVar, type), result.subst(tyVar, type), sp)
            is Constructor -> copy(tyArgs = tyArgs.map { it.subst(tyVar, type) })
            ErrorSentinel, is Unknown -> this
        }
    }

    fun substMany(subst: List<Pair<TyVar, Type>>): Type =
        subst.fold(this) { acc, (v, t) -> acc.subst(v, t) }

    companion object {
        fun v(name: String) = Var(TyVar(Name(name)))
        val Int = Constructor(Namespace.prim, Name("Int"))
        val Bool = Constructor(Namespace.prim, Name("Bool"))
        val String = Constructor(Namespace.prim, Name("String"))
        val Unit = Constructor(Namespace.prim, Name("Unit"))
    }
}

data class Scheme(val vars: List<TyVar>, val ty: Type) {
    val span: Span get() = Span(vars.firstOrNull()?.name?.span?.start ?: ty.span.start, ty.span.end)
    fun freeVars(): PersistentSet<TyVar> = ty.freeVars().removeAll(vars)
    fun unknowns(): PersistentSet<Int> = ty.unknowns()

    companion object {
        fun fromType(type: Type): Scheme = Scheme(emptyList(), type)
    }
}