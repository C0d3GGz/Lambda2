package lambda.syntax

import lambda.Scheme
import lambda.Span
import lambda.Spanned
import lambda.Type

data class SourceFile(val declarations: List<Declaration>, val span: Span) {
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
        val name: Spanned<Name>,
        val scheme: Spanned<Scheme>,
        val expr: Spanned<Expression>,
        override val span: Span
    ) : Declaration()

    data class Type(
        val name: Spanned<Name>,
        val dataConstructors: List<DataConstructor>,
        override val span: Span
    ) : Declaration()
}

data class DataConstructor(val name: Name, val fields: List<Type>, val span: Span)
