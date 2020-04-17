package lambda.syntax

import lambda.Scheme
import lambda.Span
import lambda.TyVar
import lambda.Type

data class ModuleHeader(val namespace: Namespace, val imports: List<Import>, val span: Span)

data class SourceFile(val header: ModuleHeader, val declarations: List<Declaration>, val span: Span) {
    fun typeDeclarations(): List<Declaration.Type> {
        return declarations.mapNotNull { it as? Declaration.Type }

    }

    fun valueDeclarations(): List<Declaration.Value> {
        return declarations.mapNotNull { it as? Declaration.Value }
    }
}

sealed class Declaration {

    abstract val span: Span

    data class Value(
        val name: Name,
        val scheme: Scheme,
        val expr: Expression,
        override val span: Span
    ) : Declaration()

    data class Type(
        val name: Name,
        val tyArgs: List<TyVar>,
        val dataConstructors: List<DataConstructor>,
        override val span: Span
    ) : Declaration()
}

data class DataConstructor(val name: Name, val fields: List<Type>, val span: Span)

// Namespace(listOf("Data", "List")) => Data::List
// Namespace(listOf("Option")) => Option
// Namespace(emptyList()) => local namespace
inline class Namespace(val names: List<Name>) {

    constructor(name: Name): this(listOf(name))

    val span: Span?
        get() = names.firstOrNull()?.let { first ->
            val start = first.span.start

            names.lastOrNull()?.let { last ->
                val end = last.span.end

                Span(start, end)
            }
        }

    fun isLocal() = names.isEmpty()

    fun splitType() = Namespace(names.dropLast(1)) to names.last()

    fun nest(name: Name): Namespace = Namespace(names + name)

    override fun toString(): String = names.joinToString("::")

    companion object {
        val local = Namespace(emptyList())
        val prim = Namespace(listOf(Name("Prim")))
    }
}

sealed class Import {
    abstract val span: Span
    abstract val namespace: Namespace

    // Qualified(Option, Option) => import Option | import Option as Option
    // Qualified(Data::List, L) => import Data::List as L
    // Qualified(Lambda2::AST::Expression, AST::Expression) => import Lambda2::AST::Expression as AST::Expression
    // Qualified(Lambda2::IR::Expression, IR::Expression) => import Lambda2::IR::Expression as IR::Expression
    data class Qualified(override val namespace: Namespace, val alias: Namespace, override val span: Span) : Import()
}